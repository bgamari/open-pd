import win32serviceutil
import win32service
import win32gui
import win32gui_struct
struct = win32gui_struct.struct
pywintypes = win32gui_struct.pywintypes
import win32con
import threading
import openpd

GUID_DEVINTERFACE_USB_DEVICE = "{A5DCBF10-6530-11D2-901F-00C04FB951ED}"
DBT_DEVICEARRIVAL = 0x8000
DBT_DEVICEREMOVECOMPLETE = 0x8004

def _UnpackDEV_BROADCAST(lparam):
    """
    Cut-down clone of UnpackDEV_BROADCAST from win32gui_struct, to be
    used for monkey-patching said module with correct handling
    of the "name" param of DBT_DEVTYPE_DEVICEINTERFACE
    """
    import ctypes
    
    if lparam == 0: return None
    hdr_format = "iii"
    hdr_size = struct.calcsize(hdr_format)
    hdr_buf = win32gui.PyGetMemory(lparam, hdr_size)
    size, devtype, reserved = struct.unpack("iii", hdr_buf)
    # Due to x64 alignment issues, we need to use the full format string over
    # the entire buffer.  ie, on x64:
    # calcsize('iiiP') != calcsize('iii')+calcsize('P')
    buf = win32gui.PyGetMemory(lparam, size)

    extra = {}
    if devtype == win32con.DBT_DEVTYP_DEVICEINTERFACE:
        fmt = hdr_format + "16s"
        _, _, _, guid_bytes = struct.unpack(fmt, buf[:struct.calcsize(fmt)])
        extra['classguid'] = pywintypes.IID(guid_bytes, True)
        extra['name'] = ctypes.wstring_at(lparam + struct.calcsize(fmt))
    else:
        raise NotImplementedError("unknown device type %d" % (devtype,))
    return win32gui_struct.DEV_BROADCAST_INFO(devtype, **extra)
win32gui_struct.UnpackDEV_BROADCAST = _UnpackDEV_BROADCAST

class OpenPDSvc(win32serviceutil.ServiceFramework):
    _svc_name_ = "OpenPD"
    _svc_display_name_ = "OpenPD Daemon"
    _svc_description_ = "Data acquisition daemon for OpenPD photodiode amplifier"

    def __init__(self,args):
        win32serviceutil.ServiceFramework.__init__(self,args)
        
        Filter = win32gui_struct.PackDEV_BROADCAST_DEVICEINTERFACE(
                    GUID_DEVINTERFACE_USB_DEVICE)
        self.hDevNotify = win32gui.RegisterDeviceNotification(self.ssh,
                                    Filter,win32con.DEVICE_NOTIFY_SERVICE_HANDLE)
        
        self.stopEvent = threading.Event()

    # Override the base class so we can accept additional events.
    def GetAcceptedControls(self):
        rc = win32serviceutil.ServiceFramework.GetAcceptedControls(self)
        rc |= win32service.SERVICE_CONTROL_DEVICEEVENT
        return rc

    def _log(self, message, eventID = 0xF000):
        from servicemanager import LogMsg, EVENTLOG_INFORMATION_TYPE
        LogMsg(EVENTLOG_INFORMATION_TYPE, eventID, (message,''))

    def log(self, message):
        self._log(message)
        
    def SvcOtherEx(self, control, event_type, data):
        """
        Handle non-standard service events (including our device broadcasts)
        by logging to the Application event log
        """
        if control == win32service.SERVICE_CONTROL_DEVICEEVENT:
            info = win32gui_struct.UnpackDEV_BROADCAST(data)
            if event_type == DBT_DEVICEARRIVAL:
                self.log("Device %s arrived" % info.name)
                if "VID_2323&PID_0003" in info.name:
                    self.find_devices()
                
            elif event_type == DBT_DEVICEREMOVECOMPLETE:
                self.log("Device %s removed" % info.name)
                # device removal handled by openpd.Daemon
            
    def SvcStop(self):
        self.ReportServiceStatus(win32service.SERVICE_STOP_PENDING)
        self.stopEvent.set()

    def log_started(self):
        from servicemanager import PYS_SERVICE_STARTED
        self._log(self._svc_name_, PYS_SERVICE_STARTED)

    def SvcDoRun(self):
        self.log_started()
        self.main()
        self.stopEvent.wait()

    def find_devices(self):
        import serial
        import serial.tools.list_ports as lp
        from time import sleep
        
        ports = list(lp.comports())
        for port in ports:
            portName = port[0]
            if 'OpenPD' in port[1] or port[2] == 'USB VID:PID=2323:0003':
                portUp = False
                failedToConnect = False
                sleeptime = 0
                while portUp == False and failedToConnect == False:
                    try: 
                        dev = openpd.RawOpenPD(portName)
                        self.daemon.add_device(dev)
                        portUp = True
                        self._log("Added device on port %s to OpenPD Daemon" % portName)
                    except serial.serialutil.SerialException as e:
                        # This is a hack to get the error number form the message
                        errno = int(e.message[e.message.find('WindowsError')+13])
                        if errno == 2:
                            sleep(0.5)
                            sleeptime += 0.5
                        elif errno == 5:
                            self.log("Could not connect to port %s: " % portName +\
                                     "Access is denied")
                            failedToConnect = True
                    if sleeptime >= 10:
                        failedToConnect = True
                        self.log("Timed out while trying to connect to port %s: " % portName +\
                                 "port does not exist")
                        
    def main(self):
        self.daemon = openpd.Daemon()
        self.find_devices()

        thread = threading.Thread(target=self.daemon.run)
        thread.daemon = True
        thread.start()

if __name__ == '__main__':
    win32serviceutil.HandleCommandLine(OpenPDSvc)
