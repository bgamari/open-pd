import logging
import os.path
import serial
try:
    import zmq
except:
    print "Failed to import ZeroMQ; daemon unavailable"

daemon_socket = 'tcp://127.0.0.1:9276'

class RawOpenPD(object):
    def __init__(self, device='/dev/ttyUSB.openpd'):
        """ Open a power meter device """
        self.dev = serial.Serial(device, timeout=10)
        self.dev.flushInput()
        self.dev.write('v=0\n')
        self.dev.readline()
        self.dev.write('3\n')  # force to intermediate range
        self.dev.readline()
        self.dev.write('A\n')
        self.dev.readline()
        # Force a sample
        self.sample()

    def _read_setting(self, cmd):
        self.dev.write('%s\n' % cmd)
        l = self.dev.readline()
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
        self.dev.write('w=%d\n' % wavelength)
        assert not self.dev.readline().startwith('# error')

    def get_id(self):
        """
        Get the device ID.

        :returns: a tuple containing the firmware version and the device id.
        """
        self.dev.write('?\n')
        l = self.dev.readline()
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
            self.dev.write('\n')
            for i in range(10):
                l = self.dev.readline()
                if l.startswith('#'):
                    continue
                try:
                    l = l.split()
                    rng = int(l[0])
                    power = float(l[1])
                    return {'range': rng, 'power': power}
                except:
                    pass

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

    def _command(self, cmd):
        self.sock.send_json(cmd)
        reply = self.sock.recv_json()
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
    def __init__(self, watch=True):
        self.devices = {}
        self.zmq_ctx = zmq.Context()
        self.sock = self.zmq_ctx.socket(zmq.REP)
        self.sock.bind(daemon_socket)

        if watch:
            import threading
            t = threading.Thread(target=self.watch_devices)
            t.daemon = True
            t.start()
            self.watcher = t

    def watch_devices(self):
        import pyudev
        context = pyudev.Context()
        monitor = pyudev.Monitor.from_netlink(context)
        monitor.filter_by_tag('openpd')
        for device in iter(monitor.poll):
            if device.action == 'add':
                logging.info('Trying to add device %s' % device.device_path)
                try:
                    self.add_device(device.device_path)
                except Exception as e:
                    logging.error('Failed adding device %s: %s' % (device.device_path, e))
            elif device.action == 'remove':
                logging.info('Removing device %s' % device.device_path)
                # TODO

    def add_device(self, device):
        """
        Adds a device to be handled by the daemon

        :param device: The :class:`RawOpenPD` object for the device
        """
        version, dev_id = device.get_id()
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
            reply({'devices': self.devices.keys()})
        else:
            if 'device' not in req:
                reply({'error': 'expected device'})
            elif req['device'] not in self.devices:
                reply({'error': 'unknown device'})

            device = self.devices[req['device']]
            if req_type == 'sample':
                reply(device.sample())

            elif req_type == 'set':
                for k, (_, setter) in Daemon.params:
                    if k in req:
                        setter(device, req[k])
                reply({'status': 'ok'})

            elif req_type == 'get':
                resp = {}
                for k, (getter, _) in Daemon.params:
                    resp[k] = getter(device)
                reply(resp)

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
