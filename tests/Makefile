.SILENT:

include TestList.mk

all: tests

setup:
	if [ ! -e po-hi-ada ]; then \
		echo "Checkout PolyORB-HI-Ada examples" ;\
		svn co http://eve.enst.fr/svn/polyorb-hi/trunk/examples po-hi-ada >/dev/null;\
	fi ;\
	if [ ! -e po-hi-c ]; then \
		echo "Checkout PolyORB-HI-C examples" ;\
		svn co http://eve.enst.fr/svn/polyorb_projects/design/po-hi-framework-c/examples po-hi-c >/dev/null;\
	fi

search: setup
	for v in $(SUBDIRS); do \
		if [ -f $$v/MANIFEST ]; then \
			grep Keywords: $$v/MANIFEST|grep $$key >/dev/null;\
			if [ $$? -eq 0 ]; then \
				echo $$v ;\
			fi \
		fi \
	done

tests: setup
	for v in $(SUBDIRS); do \
		$(MAKE) test name=$$v || exit 1;\
	done 

test: setup
	if [ -f $$name/MANIFEST ]; then \
		mypwd=`pwd`;\
		i=0 ;\
		nfiles=`ls -l $$name|wc -l` ;\
		while [ $$i -lt $$nfiles ]; do \
			mycmd=`grep Cmd$$i: $$name/MANIFEST|awk -F: '{print $$2}'` ;\
			if [ ! -z "$$mycmd" ]; then \
				tmp=`grep ^Output$$i: $$name/MANIFEST`;\
				if [ $$? -eq 0 ]; then \
					hasoutput=1 ;\
					myoutput=`echo $$tmp|awk -F: '{print $$2}'|sed -e 's/ //g'` ;\
					svn propget REV $$name/$$myoutput >/dev/null 2>&1 ;\
					if [ $$? -eq 0 ]; then \
						svn export $$name/$$myoutput $$name/$${myoutput}-export >/dev/null 2>&1 ;\
						myoutput=$${myoutput}-export ;\
					fi \
				else \
					hasoutput=0 ;\
				fi ;\
				tmp=`grep DiffSource$$i: $$name/MANIFEST`;\
				if [ $$? -eq 0 ]; then \
					hasdiffsource=1 ;\
					mydiffsource=`echo $$tmp|awk -F: '{print $$2}'` ;\
				else \
					hasdiffsource=0 ;\
				fi ;\
				grep "Success$$i: yes" $$name/MANIFEST >/dev/null 2>&1;\
				if [ $$? -eq 0 ]; then \
					mysuccess=1 ;\
				else \
					mysuccess=0 ;\
				fi ;\
				mycmd="$$mycmd" ;\
				cd $$name ; $$( exec $$mycmd >tmp.out 2>&1) ;\
				if [ $$? -eq 0 ]; then \
					if [ $$mysuccess -eq 0 ]; then \
						echo "FAILED TEST: $$name, command $$mycmd" && exit 1;\
					fi \
				else \
					if [ $$mysuccess -eq 1 ]; then \
						echo "FAILED TEST: $$name, command $$mycmd" && exit 1;\
					fi \
				fi ;\
				if [ $$hasoutput -eq 1 ]; then \
					if [ $$hasdiffsource -eq 1 ]; then \
						mytrace=$$mydiffsource ;\
					else \
						mytrace=tmp.out ;\
					fi ;\
					diff -urN $$myoutput $$mytrace 2>/dev/null 1>&2;\
					if [ $$? -ne 0 ]; then \
						echo "DIFF FAILED: $$name, command $$mycmd" && exit 1;\
					fi \
				fi ;\
				rm -f tmp.out ;\
				cd $$mypwd ;\
			fi ;\
			i=`expr $$i + 1`;\
		done \
	else \
		echo "$$name: This test does not exist" && exit 1;\
	fi

show: setup
	if [ -f $$name/MANIFEST ]; then \
		mydesc=`grep Description: $$name/MANIFEST |awk -F: '{print $$2}'` ;\
		mykeywords=`grep Keywords: $$name/MANIFEST|awk -F: '{print $$2}'` ;\
		echo "Test name: $$name" ;\
		echo "Description: $$mydesc" ;\
		echo "Keywords: $$mykeywords" ;\
	else \
		echo "$$name: This test does not exist" && exit 1;\
	fi
