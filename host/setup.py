#!/usr/bin/env python

from distutils.core import setup
import os

if os.sys.platform == 'linux2':
    setup(name='openpd',
          version='1.0',
          description='Library for interfacing with OpenPD laser power meter',
          author='Ben Gamari',
          author_email='ben@smart-cactus.org',
          url='https://www.github.com/bgamari/open-pd',
          py_modules=['openpd'],
          scripts=['openpd', 'openpd-daemon', 'plot-power'],
          data_files=[
              ('/etc/udev/rules.d', ['99-openpd.rules']),
              ('/lib/systemd/system', ['openpd-daemon.service']),
          ],
         )
    
elif os.sys.platform == 'win32':
    from distutils.command.install import install

    class Install(install):
        def run(self):
            install.run(self)

            # post-install
            from subprocess import check_call
            import os
            
            winsvc_path = os.path.join(self.install_base,'Scripts',
                                       'openpd-winsvc.py')
            check_call(['python',winsvc_path,'--startup','auto','install'])
            check_call(['python',winsvc_path,'restart'])
    
    setup(name='openpd',
          version='1.0',
          description='Library for interfacing with OpenPD laser power meter',
          author='Ben Gamari',
          author_email='ben@smart-cactus.org',
          url='https://www.github.com/bgamari/open-pd',
          py_modules=['openpd'],
          scripts=['openpd', 'openpd.bat', 'plot-power', 'openpd-winsvc.py'],
          cmdclass={'install': Install},
         )

else:
    raise RuntimeError("(Currently) Unsupported platform. Feel free to contribute!")
