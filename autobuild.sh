#! /bin/sh

# This script builds Ocarina automatically. It is used to regularly
# test the status of the source code.

TMP_FILE=`mktemp filXXXXXX`
TMP_DIR=`mktemp -d dirXXXXXX`

echo "SVN: reconfiguring..."
./support/reconfig > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in reconfig"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "SVN: configuring..."
./configure $@ > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in configure"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "SVN: light cleanup..."
make clean > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in make clean"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "SVN: building..."
make > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in make"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "SVN: installation..."
make install > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in make install"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "SVN: full cleanup..."
make distclean > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in make distclean"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "SVN: configuring 2..."
./configure $@ > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in configure 2"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "SVN: building distribution..."
make dist > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in make dist"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "SVN: extract archive..."
tar xzvf ocarina*.tar.gz -C ${TMP_DIR} > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in make dist"
    cat ${TMP_FILE}
    exit ${s}
fi

# Enter to the archive dir

cd ${TMP_DIR}/ocarina*

echo "DIST: configuring..."
./configure $@ > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "DIST: problem in configure"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "DIST: building..."
make > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "DIST: problem in make"
    cat ${TMP_FILE}
    exit ${s}
fi

echo "DIST: installation..."
make install > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "DIST: problem in make install"
    cat ${TMP_FILE}
    exit ${s}
fi

cd ../..

# Rebuild the SVN copy

echo "SVN: building 2..."
make > ${TMP_FILE} 2>&1
s=$?
if [ ${s} != 0 ]; then
    echo "SVN: problem in make 2"
    cat ${TMP_FILE}
    exit ${s}
fi

rm -f ${TMP_FILE}
rm -rf ${TMP_DIR}

exit 0

