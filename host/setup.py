#!/usr/bin/env python

from distutils.core import setup

setup(name='openpd',
      version='1.0',
      description='Library for interfacing with OpenPD laser power meter',
      author='Ben Gamari',
      author_email='ben@smart-cactus.org',
      url='https://www.github.com/bgamari/open-pd',
      py_modules=['openpd'],
      scripts=['openpd-acquire', 'openpd-daemon'],
      data_files=[
          ('/etc/udev/rules.d', ['99-openpd.rules']),
          ('/lib/systemd/system', ['openpd-daemon.service']),
      ],
     )
