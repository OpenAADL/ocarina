#!/bin/sh

# $Id: update_changelog.sh 4637 2008-09-24 11:57:08Z zalila $

# This script has to ve invoked from the PolyORB-HI main directory

usage () {
    cat <<EOF
Usage: ./support/`basename ${0}`
      Must be executed from the main directory
EOF
}

if ! test x"${0}" = x"./support/`basename ${0}`"; then
    usage;
    exit 1;
fi;

svn update

latest_rev=`LANG=C head ChangeLog 2>/dev/null\
    | grep '^r' \
    | head -1 \
    | tr ' ' '\n' \
    | head -1 \
    | cut -d'r' -f2`

tmp_file=`mktemp file.XXXXXX`

if test "${latest_rev}" -gt 0; then
    start_rev=`expr ${latest_rev} + 1`

    end_rev=`LANG=C svn info 2>/dev/null \
	| grep "^Revision:" \
	| awk '{print $NF}'`

    if test ${start_rev} -le ${end_rev}; then
	LANG=C svn log -v --incremental -r ${end_rev}:${start_rev} \
	    > ${tmp_file}
	cat ChangeLog >> ${tmp_file}
	mv -f ${tmp_file} ChangeLog
    fi
else
    LANG=C svn log -v > ChangeLog
fi

exit 0

