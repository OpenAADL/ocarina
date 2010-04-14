#! /bin/sh

SRC="../src"
CORE="$SRC/core"
FLAGS="-I$SRC/aadl -I$SRC/dia -I$SRC/mgmt \
-I$CORE/tree -I$CORE/model -I$CORE/instance -I$CORE/common_files"
FILES="$SRC/aadl/*.ad? $SRC/mgmt/*.ad? \
$CORE/tree/*.ad? $CORE/model/*.ad? $CORE/instance/*.ad? \
../examples/*.ad?"

if xmlada-config --version 2>&1 | ${GREP} "^XmlAda" > /dev/null 2>&1; then
    FILES="$FILES $SRC/dia/*.ad?"
    FLAGS="$FLAGS `xmlada-config --cflags`"
fi

if [ $# -gt 0 ]; then
    FILES=$*
fi

for fic in $FILES; do
    echo "reformatting $fic ..."
    gnatpp -rnb -l2 -A0 $FLAGS $fic
done