#!/bin/bash

NAME=$1
if [ "$NAME" = "" ]; then
        echo "Usage: init.sh NAME"
        exit 1;
fi;

git mv myproject.sch $NAME.sch
git rm init.sh
sed -i -e 's/myproject/$NAME/g' Makefile
git commit -a -m "Rename project to $NAME"
echo >$NAME.gsch2pcb <<EOF
schematics $NAME.sch
output-name $NAME
skip-m4
EOF

git add $NAME.gsch2pcb
mv ../skeleton-geda-project ../$NAME

echo "Done."
