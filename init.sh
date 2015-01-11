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

if [ -e ../$name ]; then
    echo "Error: ../$name already exists"
    exit 1
fi;
git clone . ../$name
cd ../$name

git rm init.sh

function process() {
    sed -i -e "s/\\myproject/$name/g" $1
    if [ $# == 2 ]; then
        git mv $1 $2
        git add $2
    else
        git add $1
    fi
}

process project
process myproject.sch $name.sch
process Makefile
process front.gvp
process back.gvp
    
git add project
git rm --cached README.mkd
mv README.mkd README.skeleton.mkd
cat >README.mkd <<EOF
$name - A project description

This project needs a description.
EOF
git commit -a -m "start project $name"
git remote rename origin skeleton-geda

echo <<EOF

Your project is now ready.

See README.skeleton.mkd for more details on how to proceed.
EOF

