#! /bin/sh

# This script compiles Ocarina and then runs it for each *.aadl file
# recursively found in the directories provided as parameters, or
# recursively in default ${root_dirs} if no parameter has been
# provided.

# If no file named *.aadl.out is present, then the output is compared
# with the input; they must match. Else the output is compared with
# *.aadl.out; this is useful if the test is meant to fail.

# The script returns the number of errors found, and indicates the
# number of unsuccessfully handled files

echo_log () {
    echo $@ | tee -a ${logfile}
}

cat_log () {
    cat $@ | tee -a ${logfile}
}

debug () {
    if test x"${ENABLE_DEBUG=yes}" = x"yes"; then
	echo "DEBUG: $1"
    fi
}

usage () {
    cat <<EOF
     Usage : $0 [options]

     Options:
       --help     |-h : This help
       --tests    |-t {<test_directories>}: Do the tests (testsuite directory)
       --examples |-x {examples_directories}: Do the examples (examples directory)
       --runtimes |-r : Test the PolyORB-HI runtimes
       --projects |-p : Test the AADL projects
       --all      |-a : Test Ocarina, runtimes and projects
       --lcov     |-l: Generate html output from gcov information
       --overwrite|-o: Overwrite reference file
       --clean    |-c: Delete intermediate files
EOF
}

banner () {
    echo "$1 $2" | \
	awk '{printf ("%.65s %s\n", $1, $2)}' | \
	awk '{printf ("%-65s%10s\n", $1, $2)}' | \
	tee -a ${logfile}
}

ignored () {
    banner $1 IGNORED
}

passed () {
    banner $1 PASSED
}

failed () {
    banner $1 FAILED
    echo_log "--------------------------- command -----------------------------"
    echo_log "$2"
    if [ -f "$3" ]; then
	echo_log "----------------------- expected output -------------------------"
	cat_log $3
	echo_log "----------------------- actual output ---------------------------"
	cat_log $4
	echo_log "-----------------------------------------------------------------"
    elif [ -f "$4" ]; then
	echo_log "--------------------------- output ------------------------------"
	cat_log $4
	echo_log "-----------------------------------------------------------------"
    fi
}

# Clean up temporary files if interrupted manually by the user

cleanup () {
    rm -rf ${tmpdir}
    rm -f  ${actual_output}
    rm -f  ${output_file}
    rm -f  ${logfile}
}

trap '{
echo "Interrupted! Cleanup..."

cd ${workdir}
cleanup
exit 1
}' 1 2 13 15

ocarina="ocarina -q"
ocarina_generate_code="ocarina -b -x"
default_flags="-g aadl"
default_version=""
workdir="`pwd`"

separator="---------------------------------------------------------------------------"

tmpdir=${workdir}/`mktemp dir.XXXXXX`
logfile=${workdir}/`mktemp log.XXXXXX`
actual_output=${workdir}/`mktemp out.XXXXXX`
output_file=${workdir}/`mktemp file.XXXXXX`

# The default value for the path converter is 'echo'. By default, we
# do not convert paths. the user may override this value by providing
# his own path converter (such as 'cygpath -w').
path_conv="${path_conv-echo}"

# The default value of the path separator is ':'. By default we use
# the UNIX path separator. The user may override this value by
# providing his own path separator (such as ';' for windows
# platforms).
path_sep="${path_sep-:}"

# Set execute mode
chmod 755 ${scriptdir}/tools/compare.py 

# If dos2unix is not present, we display a warning
if which dos2unix 2>&1 >/dev/null; then
    dos2unix="dos2unix"
else
    echo "%%%%%%%  WARNING: dos2unix not found  %%%%%%%%%"
    # Affect a dummy value
    dos2unix=":"
fi

scriptdir=`dirname $0`; cd ${scriptdir}; scriptdir="`pwd`"
cd ${workdir}

failures=0
total=0
dotests="true"
doexamples="false" # disabled for now
doruntimes="false"
doprojects="false"
doall="false"
overwrite_output="false"
testfiles=""
examplefiles=""

if test  $# != 0 ; then
    case "$1" in
	--help|-h)
	    usage
	    exit 0
	    ;;
	--overwrite|-o)
	    overwrite_output="true"
	    ;;
	--all|-a)
	    doexamples="false"
	    dotests="false"
	    doruntimes="false"
	    doprojects="false"
	    doall="true"
	    ;;
	--tests|-t)
	    doexamples="false"
	    shift
	    while test $# -gt 0
	      do
	      testfiles="${testfiles} $1"
	      shift
	    done
	    ;;
	--examples|-x)
	    dotests="false"
	    shift
	    while test $# -gt 0; do
		examplefiles=${examplefiles}" $1"
		shift
	    done
	    ;;
	--runtimes|-r)
	    doruntimes="true"
	    dotests="false"
	    doexamples="false"
	    shift
	    ;;
	--projects|-p)
	    doprojects="true"
	    dotests="false"
	    doexamples="false"
	    shift
	    ;;
	--lcov|-l)
	    lcov -d . -c -o ocarina.gcov-info -t ocarina
	    genhtml -o html -s -f ocarina.gcov-info
	    exit 0
	    ;;
	--clean|-c)
	    rm -f log.* dir.* file.* out.*
	    exit 0
	    ;;
	*)
	    usage
	    exit 1
    esac
fi

if test ${doruntimes} = "true" ; then
    for e in "resources/runtime/polyorb-hi-c" "resources/runtime/polyorb-hi-ada"; do
	entry=${e}
	old_dir=`pwd`
	cd ${e};

	make examples 2> /dev/null > /dev/null
	if test $? != 0 ; then
	    failed ${entry} "${command}" "" "${actual_output}"
	    failures=`expr ${failures} + 1`

	else
	    passed ${entry}
	fi;

	cd ${old_dir}
    done
fi

if test ${doprojects} = "true" ; then
    old_dir=`pwd`
    cd ../../projects

    ./build.sh 2> /dev/null > /dev/null
    if test $? != 0 ; then
	banner "Projects" FAILED
	failures=`expr ${failures} + 1`

    else
	passed ${entry}
    fi;

    cd ${old_dir}
fi

if test ${doall} = "true" ; then
    ./autotest.sh 2> /dev/null > /dev/null
    if test $? != 0 ; then
	banner "Ocarina_Core" FAILED
	failures=`expr ${failures} + 1`

    else
	banner "Ocarina_Core" PASSED
    fi;

    ./autotest.sh -r 2> /dev/null > /dev/null
    if test $? != 0 ; then
	banner "Runtimes" FAILED
	failures=`expr ${failures} + 1`

    else
	banner "Runtimes" PASSED
    fi;

    ./autotest.sh -p 2> /dev/null > /dev/null
    if test $? != 0 ; then
	banner "Projects" FAILED
	failures=`expr ${failures} + 1`

    else
	banner "Projects" PASSED
    fi;
fi


if test ${doexamples} = "true" ; then
    if test "x${examplefiles}" = "x" ; then
	if test ! -f  ${scriptdir}/examples/MANIFEST; then
	    echo "${scriptdir}/examples/MANIFEST: file not found";
	    exit 2;
	fi
	examplefiles=`cat ${scriptdir}/examples/MANIFEST | grep -v "^#"`
    fi

    for e in ${examplefiles}; do
	entry=examples/${e}
	file=${scriptdir}/${entry}
	if test ! -f ${file} ; then
	    echo "FATAL : example ${e} does not exist"
	    cleanup
	    exit 1;
	fi

	rm -f  ${output_file} >/dev/null 2>&1
	rm -f  ${actual_output} >/dev/null 2>&1
	rm -rf ${tmpdir} >/dev/null 2>&1
	mkdir  ${tmpdir}
	chmod u+rwx ${tmpdir}
	cd ${tmpdir};

	total=`expr ${total} + 1`
	command="${ocarina_generate_code} \"`${path_conv} ${file}`\""
	${ocarina_generate_code} "`${path_conv} ${file}`" > ${actual_output} 2>&1
	if test $? != 0 ; then
	    failed ${entry} "${command}" "" "${actual_output}"
	    failures=`expr ${failures} + 1`

	else
	    passed ${entry}
	fi;

	cd ..
    done
fi

if test ${dotests} = "true" ; then
    if test "x${testfiles}" = "x" ; then
	if test ! -f  ${scriptdir}/tests/MANIFEST; then
	    echo "${scriptdir}/tests/MANIFEST: file not found";
	    exit 2;
	fi
	testfiles=`cat ${scriptdir}/tests/MANIFEST | grep -v "^#"`
    fi

    for t in ${testfiles}; do
	if test ! -d ${scriptdir}/${t} && test ! -f ${scriptdir}/${t};
	then
	    echo "FATAL: tests ${t} does not exist"
	    cleanup
	    exit 1;
	fi

	rm -f  ${output_file} >/dev/null 2>&1
	rm -f  ${actual_output} >/dev/null 2>&1
	cd ${workdir}
	rm -rf ${tmpdir} >/dev/null 2>&1
	mkdir  ${tmpdir}
	chmod u+rwx ${tmpdir}
	cd ${tmpdir};

	files=`find ${scriptdir}/${t} -name '*.aadl'`;

	for file in ${files}; do
	    entry=`echo ${file} | sed "s,^${scriptdir}/,,"`
            manifest="`dirname ${file}`/MANIFEST"
	    expected_output="${file}.out"
	    test_ignored="${file}.ignore"

	    # copy REAL files if such file exists

	    real_lib_base=`dirname ${file}`
	    real_libs_nb=`ls ${real_lib_base} | grep "\.real" | wc -l` > /dev/null
	    if [ ${real_libs_nb} -gt 0 ]
		then
		real_lib=`dirname ${file}`/"*.real"
		cp -f ${real_lib} .
	    fi

	    flags=${default_flags}
	    version=${default_version}
            if test -r ${manifest}; then
		flags=`grep OCARINA_FLAGS ${manifest} | sed  's/OCARINA_FLAGS=//'`
		version=`grep AADL_VERSION ${manifest} | awk -F= '{print $2}'`
		flags=${flags:-${default_flags}}
		version=${version:-${default_version}}
            fi

	    if test -r ${test_ignored} ; then
		ignored "${entry}"

	    else
		touch ${output_file}
		${ocarina} ${flags} ${version} "`${path_conv} ${file}`" \
		    -o "`${path_conv} ${output_file}`" > ${actual_output} 2>&1
		if test -r ${expected_output} ; then
		    cat ${output_file} >>${actual_output}
		    ${dos2unix} ${actual_output} >/dev/null 2>&1
		    ${scriptdir}/tools/compare.py \
			${expected_output} \
			${actual_output} >/dev/null 2>&1
		    result=$?

		else
		    ${scriptdir}/tools/compare.py \
			${file} \
			${output_file} >> /dev/null 2>&1
		    result=$?
		fi

		if test  ${result} != 0 ; then
		    if test -r ${expected_output} ; then
			failed ${entry} \
			    "${ocarina} ${flags} ${version} \"`${path_conv} ${file}`\"" \
			    ${expected_output} \
			    ${actual_output}

			if test ${overwrite_output} = "true"; then
			    cp ${actual_output} ${expected_output}
			fi

		    else
			failed ${entry} \
			    "${ocarina} ${flags} ${version} \"`${path_conv} ${file}`\"" \
			    ${file} \
			    ${output_file}
		    fi
		    failures=`expr ${failures} + 1`

		else
		    passed ${entry}
		fi
		total=`expr ${total} + 1`
	    fi
	done

	files=`find ${scriptdir}/${t} -name '*.adb'`;

	for file in ${files}; do
	    entry=`echo ${file} | sed "s,^${scriptdir}/,,"`
	    expected_output="${file}.out"
	    ignored_output="${file}.ignore"
	    gprfile="`basename ${file} .adb`.gpr"
	    gprfile="`dirname ${file}`/${gprfile}"

	    cd ${tmpdir}

	    if test -r ${ignored_output} ; then
		ignored ${entry}

	    else
		total=`expr ${total} + 1`

		case "$(uname -s)" in

		    CYGWIN*|MINGW32*|MSYS*)
			ignored ${entry}
			;;

		    *)
			
			if test -r ${gprfile} ; then
			    ocarina_gpr="`ocarina-config --projects`"
			    command="gnatmake -P\"`${path_conv} ${gprfile}`\" -aP${ocarina_gpr} -XOBJ_DIR=\"`${path_conv} ${tmpdir}`\""
			    ADA_PROJECT_PATH="${ocarina_gpr}${path_sep}${ADA_PROJECT_PATH}" \
					    gnatmake -P"`${path_conv} ${gprfile}`" -aP${ocarina_gpr} \
					    -XOBJ_DIR="`${path_conv} ${tmpdir}`" \
					    >${actual_output} 2>&1
			else
			    command="gnatmake '`${path_conv} ${file}`' `ocarina-config`"
			    gnatmake "`${path_conv} ${file}`" `ocarina-config` \
				     >${actual_output} 2>&1
			fi
			
			if test $? != 0 ; then
			    failed ${entry} \
				   "${command}" \
				   "" \
				   "${actual_output}"
			    failures=`expr ${failures} + 1`
			    
			else
			    ./`basename ${file} .adb` >${actual_output} 2>&1
			    command="./`basename ${file} .adb`"
			    result=$?
			    
			    if test -r ${expected_output} ; then
				${scriptdir}/tools/compare.py \
					    ${expected_output} \
					    ${actual_output} > /dev/null
				result=$?
			    fi;
			    
			    if test ${result} != 0 ; then
				if test -r ${expected_output} ; then
				    failed ${entry} \
					   "${command}" \
					   ${expected_output} \
					   ${actual_output}
				    
				else
				    failed ${entry} \
					   "${command}"
				fi
				failures=`expr ${failure} + 1`
				
			    else
				passed ${entry}
			    fi
			fi;;
		esac
	    fi
	done
    done
fi

echo_log ${separator}
echo_log "FAILURES : ${failures} / ${total}"
echo_log ${separator}

if test ${failures} != 0; then
    echo "Result trace is stored in ${logfile}"
    cd "${workdir}"
    rm -fr "${actual_output}"
    rm -fr "${output_file}"
    rm -fr "${tmpdir}"
else
    cd "${workdir}"
    cleanup
fi

exit ${failures}
