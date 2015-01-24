#!/bin/bash

( while true; do echo > /dev/ttyACM0; sleep 10; done; )&
cat /dev/ttyACM0
