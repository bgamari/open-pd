#!/bin/bash

NAME=$1
if [ "$NAME" = "" ]; then
        echo
        exit 1;
fi;

git mv project.sch $NAME.sch
git mv project.pcb $NAME.pcb
git rm start.sh
git commit -a -m "Rename project to $NAME"

echo "Done."
