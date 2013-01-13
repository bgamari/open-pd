#!/bin/bash

echo <<EOF
    init.sh - skeleton-geda-project

This script is used to begin a gEDA project. After specifying a
project name, the script will initialize the project configuration.

EOF

echo "Enter a name for your project: "
read name
if [ "$name" = "" ]; then
    echo "Error: Need a non-empty name for the project."
    exit 1
fi;


git mv myproject.sch $name.sch
git rm init.sh
sed -i -e 's/myproject/$name/g' Makefile

echo >$name.gsch2pcb <<EOF
schematics $NAME.sch
output-name $name
skip-m4
EOF
git add $name.gsch2pcb

git commit -a -m "rename project to $name"

git add $name.gsch2pcb
mv ../skeleton-geda-project ../$name

echo <<EOF

Your project is now ready.

See README.mkd for more details on how to proceed.
EOF

