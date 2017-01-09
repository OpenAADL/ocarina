#! /bin/sh
# $Id: get_runtimes.sh 7229 2010-03-01 11:17:32Z zalila $

usage () {
    cat <<EOF
       Usage: `basename $0` [runtime1 [runtime2 [...]]]

       Download the specified runtimes and place them in
       resources/runtime

       Examples: $0 pohiada
                 $0 PolyORB-HI-Ada
                 $0 po_hi_ada po_hi_c pok

       IMPORTANT: Any pre-existing runtimes will be overriden
EOF
}

if test $# -eq 0; then
    usage
    exit 1
fi

repository="https://github.com/OpenAADL"
tag=""

workdir="`pwd`"
scriptdir="`dirname $0`"; cd "${scriptdir}"; scriptdir="`pwd`"
cd ${workdir}

while test $# -ne 0; do
    r="`echo $1 | tr '[A-Z]' '[a-z]'`"

    case ${r} in
        --root_url=*) repository=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;

        --tag=*) tag=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;

        *aadlib* )
	    cd ${scriptdir}/../resources/runtime || exit 2
	    rm -rf AADLib 2>/dev/null
	    git clone ${repository}/AADLib.git aadlib \
		|| exit 2
            if test ! -z "${tag}"; then
                git checkout ${tag} -b ${tag}
            fi;
	    ;;

	*po*hi*ada* )
	    cd ${scriptdir}/../resources/runtime || exit 2
	    rm -rf polyorb-hi-ada 2>/dev/null
	    git clone ${repository}/polyorb-hi-ada.git \
		|| exit 2
            if test ! -z "${tag}"; then
                git checkout ${tag} -b ${tag}
            fi;
	    ;;

	*po*hi*c* )
	    cd ${scriptdir}/../resources/runtime || exit 2
	    rm -rf polyorb-hi-c 2>/dev/null
	    git clone ${repository}/polyorb-hi-c.git \
		|| exit 2
            if test ! -z "${tag}"; then
                git checkout ${tag} -b ${tag}
            fi;
	    ;;

	*pok* )
	    cd ${scriptdir}/../resources/runtime || exit 2
	    rm -rf pok 2>/dev/null
            git clone https://github.com/pok-kernel/pok.git \
		|| exit 2
	    ;;

	* )
	    echo "Unknown runtime: '${1}'"
	    exit 2
	    ;;
    esac
    shift
done

exit 0
