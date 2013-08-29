#!/bin/sh

# $Id: update_headers.sh 4374 2008-07-04 12:50:36Z hugues $

usage () {
    cat <<EOF
Usage: ${0} [OPTIONS]
Options:
        No option:
           Update the Copyright headers only for the newly added files and
           the modified files (using SVN).

        [all]
           Updates the Copyright headers of all files.

Note: this script has to be invoked from the Ocarina main directory.
EOF
}

work_dir="`pwd`"
script_dir=`dirname $0`; cd ${script_dir}; script_dir="`pwd`"
cd ${work_dir}

if test $# = 0 ; then
    echo "Updating headers for changed and new files"

    changed_files=`git status -s | grep "^\ M" | grep '\.\(\(ad[bs]\)\|\(gpr\)\)' | awk '{print $NF}'`

    added_files=`git status -s | grep "^[A]" | grep '\.\(\(ad[bs]\)\|\(gpr\)\)' | awk '{print $NF}'`

    # For changed files, update the header

    for i in ${changed_files}; do
	${script_dir}/headers_ocarina ${i}
    done

    # For added files, create a new header

    for i in ${added_files}; do
	${script_dir}/headers_ocarina -noh ${i} > /dev/null 2>&1
	${script_dir}/headers_ocarina ${i}
    done
else
    if test x${1} = xall ; then
	echo "Updating headers for all files"

	find . -name "*.adb" -exec ${script_dir}/headers_ocarina $1 '{}' \;
	find . -name "*.ads" -exec ${script_dir}/headers_ocarina $1 '{}' \;
	find . -name "*.gpr" -exec ${script_dir}/headers_ocarina $1 '{}' \;
    else
	usage 1>&2
	exit 1
    fi
fi

exit 0
