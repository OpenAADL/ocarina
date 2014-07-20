#!/bin/sh

for file in `ls projects/*-*.gpr tools/mknodes/mknodes.gpr`; do
    echo "Processing $file"
    ADA_PROJECT_PATH=projects:$ADA_PROJECT_PATH && gnatpp -rnb -P$file;
done