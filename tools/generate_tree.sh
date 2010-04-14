#! /bin/sh

# This script generates AST manipulation Ada code from a pseudo IDL
# description of the tree. If the first parameter is "hard", the
# script exits with an error code if an error occurs. If the first
# parameter is "soft", the script exits with an error code if and only
# if the pseudo IDL file exists and contains errors. The directories
# to the mknodes and headers_ocarina executables are passed via
# environment variables.

usage () {
    cat << EOF
Usage: $0 (hard|soft) <Pseudo_IDL_File>
   hard: If the Ada files cannot be generated, exit with an error code
   soft: Do no exist with error code only if the pseudo IDL file exists
         and contains errors.

   Some environment variables are necessary:
    - MKNODES_DIR  : directory that hols the mknodes executable
    - UHEADERS_DIR : Directory that holds the copyright headers updater
    - OUTPUT_DIR   : Directory where the Ada files are generated
    - GNU_MAKE     : The GNU Make command
    - CYGPATH_W    : The equivalent of cygpath -w
EOF
    exit 1
}

if test $# -ne 2 ; then
    usage;
fi;

if test x"${MKNODES_DIR}" = x"" \
    -o x"${UHEADERS_DIR}" = x"" \
    -o x"${OUTPUT_DIR}" = x"" \
    -o x"${GNU_MAKE}" = x"" \
    -o x"${CYGPATH_W}" = x"";
then
    usage;
fi

# Get absolute paths for the script and the argument

current_dir=`pwd`

script_dir="`dirname $0`"; cd ${script_dir}; script_dir=`pwd` 
cd ${current_dir}

idl_tree_dir="`dirname $2`"; cd ${idl_tree_dir}; idl_tree_dir=`pwd`
cd ${current_dir}

mknodes_dir=${MKNODES_DIR}; cd ${mknodes_dir};  mknodes_dir=`pwd`
cd ${current_dir}

uheaders_dir=${UHEADERS_DIR}; cd ${uheaders_dir};  uheaders_dir=`pwd`
cd ${current_dir}

idl_file="`basename $2`"
tree_ada_spec="`basename $2 .idl`.ads"
tree_ada_body="`basename $2 .idl`.adb"

tmp=nodes.tmp
tmp_dir=temp.$$
mknodes="${mknodes_dir}/mknodes"
update_headers="${uheaders_dir}/headers_ocarina"

# Tests whether mknodes exists

if test -x ${mknodes} ; then
    mknodes_exists=yes
else
    mknodes_exists=no
fi;

# Tests whether the pseudo IDL file exists

if test -f "${idl_tree_dir}/${idl_file}" ; then
    idl_file_exists=yes
else
    idl_file_exists=no
fi;

# Tests whether update_headers exists

# Compile ${update_headers} if possible

uheaders_src="${script_dir}/../support/`basename ${update_headers}`.adb"
echo $uheaders_src

if test -d ${uheaders_dir} && \
    test -r ${uheaders_dir}/Makefile && \
    test -r ${uheaders_src} ; then
    ${GNU_MAKE} -C ${uheaders_dir} "`basename ${update_headers}`"
fi

if test -x ${update_headers} ; then
    update_headers_exists=yes
else
    update_headers_exists=no
fi;

# Test if all is OK

ALL_OK=yes

(test "${mknodes_exists}" = "yes")        || ALL_OK=no
(test "${idl_file_exists}" = "yes")       || ALL_OK=no

if test x${ALL_OK} = xno ; then
    if test x$1 = xhard ; then
	if test x${mknodes_exists} = xno ; then
	    echo "mknodes : not found"
	fi

	if test x${idl_file_exists} = xno ; then
	    echo "${idl_file} : not found"
	fi
	  
	# Exit with an error status

	exit 1;
    else
	# Exit silently

	exit 0;
    fi
fi

# Check whether ${update_headers} exists

if test x${update_headers_exists} = xno ; then
    if test x$1 = xhard ; then
	echo "${update_headers} not found"

        # Exit with an error status
	
	exit 1;
    fi
fi

# Process the pseudo IDL file in a temporary directory

echo "Processing ${idl_file}"

cd ${OUTPUT_DIR} 

mkdir ${tmp_dir} || exit $?

echo "${mknodes} `${CYGPATH_W} ${idl_tree_dir}/${idl_file}`" \
    " -D `${CYGPATH_W} ${tmp_dir}`;" 
"${mknodes}" "`${CYGPATH_W} ${idl_tree_dir}/${idl_file}`" \
    -D "`${CYGPATH_W} ${tmp_dir}`"
EXIT_CODE=$?

# Update the copyright headers (if possible)
# FIXME: Deactivated for now because it generates files with 
# incompatible line terminator characters on windows.

#if test x${update_headers_exists} = xyes ; then
#    "${update_headers}" "`${CYGPATH_W} ${tmp_dir}/${tree_ada_spec}`"
#    "${update_headers}" "`${CYGPATH_W} ${tmp_dir}/${tree_ada_body}`"
#fi

# Replace the tree files if they are different from the original files

for file in ${tmp_dir}/${tree_ada_spec} ${tmp_dir}/${tree_ada_body} ; do
    ofile="`basename ${file}`"
    if test -r "${ofile}"  ; then
	if cmp "${file}" "${ofile}" > /dev/null; then
	    rm -f "${file}"
	else
	    mv -f "${file}" "${ofile}"
	fi
    else
	mv -f "${file}" "${ofile}"
    fi
done

rm -rf "${tmp_dir}"

cd "${current_dir}"
exit "${EXIT_CODE}"
