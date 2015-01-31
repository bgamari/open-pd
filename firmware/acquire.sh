#!/bin/bash -e

delay=$1
if [ "$delay" == "" ]; then delay=0.5; fi
if [ "$device" == "" ]; then device=/dev/ttyACM0; fi 

( while true; do echo > $device; sleep $delay; done; )&
cat $device
