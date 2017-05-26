import logging
import os.path
import serial
import sys
try:
    import zmq
except:
    print("Failed to import ZeroMQ; daemon unavailable")

daemon_socket = 'tcp://127.0.0.1:9276'

class RawOpenPD(object):
    def __init__(self, device='/dev/ttyUSB.openpd'):
        """ Open a power meter device """
        self.dev = serial.Serial(device, timeout=10)
        self.dev.flushInput()
        self.dev.write(b'v=0\n')
        self.dev.readline()
        self.force_range(3)  # force to intermediate range
        self.set_auto_range(True)
        # Force a sample
        self.sample()

    @property
    def device_path(self):
        """
        The path to the underlaying port device
        """
        return self.dev.name

    def _read_setting(self, cmd):
        if not type(cmd) == bytes: cmd = cmd.encode('utf-8')
        self.dev.write(b'%s\n' % cmd)
        l = self.dev.readline().decode('utf-8')
        return l.split('=')[1]

    def get_wavelength(self):
        """
        Retrieve the configured wavelength
        """
        return self._read_setting('w')

    def set_wavelength(self, wavelength):
        """
        Sets the configured wavelength

        :param wavelength: Wavelength in nanometers
        :type wavelength: :class:`int`
        """
        self.dev.write(b'w=%d\n' % wavelength)
        assert not self.dev.readline().startswith(b'# error')

    def force_range(self, rng):
        """
        Force the gain range of the device
        """
        assert rng in range(8)
        self.dev.write(b'%d\n' % rng)
        assert not self.dev.readline().startswith(b'# error')

    def set_auto_range(self, on):
        """
        Enable/disable autoranging
        """
        self.dev.write(b'A\n' if on else b'a\n')
        assert not self.dev.readline().startswith(b'# error')

    def get_id(self):
        """
        Get the device ID.

        :returns: a tuple containing the firmware version and the device id.
        """
        self.dev.write(b'?\n')
        l = self.dev.readline().decode('utf-8')
        parts = l.split()
        if parts[1] != 'OpenPD':
            raise RuntimeError('Invalid response to ? command')

        version = parts[2]
        dev_id = parts[3]
        return (version, dev_id)

    def sample(self):
        """ Sample the power """
        # How I wish this weren't so brittle
        while True:
            logging.info('Requesting a sample')
            self.dev.write(b'\n')
            for i in range(10):
                l = self.dev.readline().decode('utf-8')
                logging.info('Read a sample')

                if l.startswith('#'):
                    continue

                l = l.split()
                if len(l) < 2:
                    continue
                rng = int(l[0])
                power = float(l[1])
                return {'range': rng, 'power': power}

class OpenPDError(Exception):
    def __init__(self, error):
        self.error = error

    def __str__(self):
        return str(error)

class Connection(object):
    """ Connection to daemon """
    def __init__(self):
        self.zmq_ctx = zmq.Context()
        self.sock = self.zmq_ctx.socket(zmq.REQ)
        self.sock.connect(daemon_socket)
	self.sock.setsockopt(zmq.RCVTIMEO,2000)

    def _command(self, cmd):
        self.sock.send_json(cmd)
        try:
            reply = self.sock.recv_json()
        except zmq.error.Again:
            raise RuntimeError('No response from daemon. It is likely not running.')
        if 'error' in reply:
            raise RuntimeError(reply['error'])
        else:
            return reply

    def list_devices(self):
        """ Return a list of devices known by the daemon """
        return self._command({'type': 'list-devices'})['devices']

    def device(self, device):
        """ Return an :class:`OpenPD` object representing the given device """
        return OpenPD(self, device)

class OpenPD(object):
    def __init__(self, conn, device='/dev/ttyUSB.openpd'):
        self.conn = conn
        self.device = device

    def _command(self, cmd):
        cmd['device'] = self.device
        return self.conn._command(cmd)

    def get_wavelength(self):
        return self._command({'type': 'get'})['wavelength']

    def set_wavelength(self, wavelength):
        return self._command({'type': 'set', 'wavelength': wavelength})

    def sample(self):
        return self._command({'type': 'sample'})

class Daemon(object):
    def __init__(self, find_devices=True, watch=True):
        """
        :type find_devices: :class:`bool`
        :param find_devices: Whether to call :func:`find_devices` during initialization
        :type watch: :class:`bool`
        :param watch: Whether to monitor the system for new devices
        """
        self.devices = {}
        if find_devices:
            self.find_devices()

        self.zmq_ctx = zmq.Context()
        self.sock = self.zmq_ctx.socket(zmq.REP)
        self.sock.bind(daemon_socket)

        if watch:
            import threading
            t = threading.Thread(target=self.watch_devices)
            t.daemon = True
            t.start()
            self.watcher = t

    def find_devices(self):
        import sys
        if sys.platform == 'linux2':
            import pyudev
            context = pyudev.Context()
            for device in iter(context.list_devices(tag='openpd')):
                try:
                    self.add_device(RawOpenPD(device.device_node))
                except Exception as e:
                    logging.warn('Exception while trying to add device %s: %s' % (device.device_node, e))
        else:
            logging.info("Don't know how to find devices for platform %s" % sys.platform)


    def watch_devices(self):
        if sys.platform == 'linux2':
            self.watch_devices_linux()
        else:
            logging.info("Don't know how to watch for devices on platform %s" % sys.platform)

    def watch_devices_linux(self):
        import pyudev
        context = pyudev.Context()
        monitor = pyudev.Monitor.from_netlink(context)
        monitor.filter_by_tag('openpd')
	logging.info('Begin monitoring for udev notification')
        for device in iter(monitor.poll, None):
            logging.info('udev: got a device %s' % device)
            if device.action == 'add':
                try:
                    self.add_device(RawOpenPD(device.device_node))
                except Exception as e:
                    logging.error('Failed adding device %s: %s' % (device.device_path, e))
            elif device.action == 'remove':
                logging.info('Removing device %s' % device.device_path)
                s = device['ID_SERIAL_SHORT']
                dev_id = s[16:]+s[10:16]+s[:8]+'ffffffff'
                if dev_id in self.devices:
                    self.devices.pop(dev_id)
                

    def add_device(self, device):
        """
        Adds a device to be handled by the daemon

        :param device: The :class:`RawOpenPD` object for the device
        """
        logging.info('Trying to add device %s' % device.device_path)
        version, dev_id = device.get_id()
        if dev_id not in self.devices:
            self.devices[dev_id] = device

    params = {
        'wavelength': (RawOpenPD.get_wavelength, RawOpenPD.set_wavelength),
    }

    def _handle_request(self, req):
        req_type = req.get('type')
        reply = self.sock.send_json
        if req_type is None:
            reply({'error': 'malformed request'})
        elif req_type == 'list-devices':
            reply({'devices': list(self.devices.keys())})
        else:
            if 'device' not in req:
                reply({'error': 'expected device'})
            elif req['device'] not in self.devices:
                reply({'error': 'unknown device'})

            device = self.devices[req['device']]
            try:
                if req_type == 'sample':
                    reply(device.sample())

                elif req_type == 'set':
                    for k, (_, setter) in list(Daemon.params.items()):
                        if k in req:
                            setter(device, req[k])
                    reply({'status': 'ok'})

                elif req_type == 'get':
                    resp = {}
                    for k, (getter, _) in list(Daemon.params.items()):
                        resp[k] = getter(device)
                    reply(resp)
            except IOError as e:
                logging.error('Saw IOError for device %s, removing' % req['device'])
                reply({'error': 'IO error'})
                self.devices.pop(req['device'])
                # In case the event dropped out earlier but has since returned
                self.find_devices()

    def run(self):
        """
        Run the daemon's handler loop
        """
        while True:
            try:
                req = self.sock.recv_json()
                self._handle_request(req)
            except Exception as e:
                self.sock.send_json({'error': str(e)})
