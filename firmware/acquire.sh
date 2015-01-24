#!/bin/bash

delay=$1

( while true; do echo > /dev/ttyACM0; sleep $delay; done; )&
cat /dev/ttyACM0
