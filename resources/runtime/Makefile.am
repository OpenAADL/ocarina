SUBDIRS = cheddar aadl_xml ${PYTHON_RUNTIME}

@DEBUG_FALSE@DEBUG_FLAG = --disable-debug
@DEBUG_TRUE@DEBUG_FLAG = --enable-debug

################################################################################

maybe-reconfig-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-reconfig-polyorb-hi-ada: reconfig-polyorb-hi-ada
reconfig-polyorb-hi-ada:
	r_dir='polyorb-hi-ada'; \
	test -f $(srcdir)/$${r_dir}/configure && exit 0 || true; \
	test -d $(srcdir)/$${r_dir} || exit 0; \
	cd $(srcdir)/$${r_dir} && ./support/reconfig

maybe-reconfig-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-reconfig-polyorb-hi-c: reconfig-polyorb-hi-c
reconfig-polyorb-hi-c:
	r_dir='polyorb-hi-c'; \
	test -f $(srcdir)/$${r_dir}/configure && exit 0 || true; \
	test -d $(srcdir)/$${r_dir} || exit 0; \
	cd $(srcdir)/$${r_dir} && ./support/reconfig

maybe-reconfig-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-reconfig-polyorb-hi-java: reconfig-polyorb-hi-java
reconfig-polyorb-hi-java:
	r_dir='polyorb-hi-java'; \
	test -f $(srcdir)/$${r_dir}/configure && exit 0 || true; \
	test -d $(srcdir)/$${r_dir} || exit 0; \
	cd $(srcdir)/$${r_dir} && ./support/reconfig

maybe-reconfig-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-reconfig-polyorb-qos-ada: reconfig-polyorb-qos-ada
reconfig-polyorb-qos-ada:
	r_dir='polyorb-qos-ada'; \
	test -f $(srcdir)/$${r_dir}/configure && exit 0 || true; \
	test -d $(srcdir)/$${r_dir} || exit 0; \
	cd $(srcdir)/$${r_dir} && ./support/reconfig

################################################################################

maybe-configure-pok:
@POK_TRUE@maybe-configure-pok: configure-pok
configure-pok:
	r_dir='pok'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) configure

maybe-configure-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-configure-polyorb-hi-ada: configure-polyorb-hi-ada
configure-polyorb-hi-ada: reconfig-polyorb-hi-ada
	r_dir='polyorb-hi-ada'; \
	s_dir=`(cd $(srcdir) && pwd)`; \
	test -f $${r_dir}/Makefile && exit 0 || true; \
	test -x $(srcdir)/$${r_dir}/configure || exit 0; \
	$(mkdir_p) $${r_dir} && cd $${r_dir} || exit 1; \
	$${s_dir}/$${r_dir}/configure $(DEBUG_FLAG) $(DOC_FLAG) \
	   --with-ocarina-prefix='$(prefix)' --disable-ocarina-version-check

maybe-configure-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-configure-polyorb-hi-c: configure-polyorb-hi-c
configure-polyorb-hi-c: reconfig-polyorb-hi-c
	r_dir='polyorb-hi-c'; \
	s_dir=`(cd $(srcdir) && pwd)`; \
	test -f $${r_dir}/Makefile && exit 0 || true; \
	test -x $(srcdir)/$${r_dir}/configure || exit 0; \
	$(mkdir_p) $${r_dir} && cd $${r_dir} || exit 1; \
	$${s_dir}/$${r_dir}/configure $(DEBUG_FLAG) $(DOC_FLAG) \
	   --with-ocarina-prefix='$(prefix)' --disable-ocarina-version-check

maybe-configure-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-configure-polyorb-hi-java: configure-polyorb-hi-java
configure-polyorb-hi-java: reconfig-polyorb-hi-java
	r_dir='polyorb-hi-java'; \
	s_dir=`(cd $(srcdir) && pwd)`; \
	test -f $${r_dir}/Makefile && exit 0 || true; \
	test -x $(srcdir)/$${r_dir}/configure || exit 0; \
	$(mkdir_p) $${r_dir} && cd $${r_dir} || exit 1; \
	$${s_dir}/$${r_dir}/configure $(DEBUG_FLAG) $(DOC_FLAG) \
	   --with-ocarina-prefix='$(prefix)' --disable-ocarina-version-check

maybe-configure-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-configure-polyorb-qos-ada: configure-polyorb-qos-ada
configure-polyorb-qos-ada: reconfig-polyorb-qos-ada
	r_dir='polyorb-qos-ada'; \
	s_dir=`(cd $(srcdir) && pwd)`; \
	test -f $${r_dir}/Makefile && exit 0 || true; \
	test -x $(srcdir)/$${r_dir}/configure || exit 0; \
	$(mkdir_p) $${r_dir} && cd $${r_dir} || exit 1; \
	$${s_dir}/$${r_dir}/configure $(DEBUG_FLAG) $(DOC_FLAG) \
	   --with-ocarina-prefix='$(prefix)' --disable-ocarina-version-check

################################################################################

maybe-all-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-all-polyorb-hi-ada: all-polyorb-hi-ada
all-polyorb-hi-ada: configure-polyorb-hi-ada
	r_dir='polyorb-hi-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) all

maybe-all-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-all-polyorb-hi-c: all-polyorb-hi-c
all-polyorb-hi-c: configure-polyorb-hi-c
	r_dir='polyorb-hi-c'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) all

maybe-all-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-all-polyorb-hi-java: all-polyorb-hi-java
all-polyorb-hi-java: configure-polyorb-hi-java
	r_dir='polyorb-hi-java'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) all

maybe-all-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-all-polyorb-qos-ada: all-polyorb-qos-ada
all-polyorb-qos-ada: configure-polyorb-qos-ada
	r_dir='polyorb-qos-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) all

################################################################################

maybe-dvi-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-dvi-polyorb-hi-ada: dvi-polyorb-hi-ada
dvi-polyorb-hi-ada: configure-polyorb-hi-ada
	r_dir='polyorb-hi-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) dvi

maybe-dvi-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-dvi-polyorb-hi-c: dvi-polyorb-hi-c
dvi-polyorb-hi-c: configure-polyorb-hi-c
	r_dir='polyorb-hi-c'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) dvi

maybe-dvi-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-dvi-polyorb-hi-java: dvi-polyorb-hi-java
dvi-polyorb-hi-java: configure-polyorb-hi-java
	r_dir='polyorb-hi-java'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) dvi

maybe-dvi-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-dvi-polyorb-qos-ada: dvi-polyorb-qos-ada
dvi-polyorb-qos-ada: configure-polyorb-qos-ada
	r_dir='polyorb-qos-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) dvi

################################################################################

maybe-install-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-install-polyorb-hi-ada: install-polyorb-hi-ada
install-polyorb-hi-ada: all-polyorb-hi-ada
	r_dir='polyorb-hi-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" install

maybe-install-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-install-polyorb-hi-c: install-polyorb-hi-c
install-polyorb-hi-c: all-polyorb-hi-c
	r_dir='polyorb-hi-c'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" install

maybe-install-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-install-polyorb-hi-java: install-polyorb-hi-java
install-polyorb-hi-java: all-polyorb-hi-java
	r_dir='polyorb-hi-java'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" install

maybe-install-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-install-polyorb-qos-ada: install-polyorb-qos-ada
install-polyorb-qos-ada: all-polyorb-qos-ada
	r_dir='polyorb-qos-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" install

################################################################################

maybe-installcheck-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-installcheck-polyorb-hi-ada: installcheck-polyorb-hi-ada
installcheck-polyorb-hi-ada: all-polyorb-hi-ada
	r_dir='polyorb-hi-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) installcheck

maybe-installcheck-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-installcheck-polyorb-hi-c: installcheck-polyorb-hi-c
installcheck-polyorb-hi-c: all-polyorb-hi-c
	r_dir='polyorb-hi-c'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) installcheck

maybe-installcheck-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-installcheck-polyorb-hi-java: installcheck-polyorb-hi-java
installcheck-polyorb-hi-java: all-polyorb-hi-java
	r_dir='polyorb-hi-java'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) installcheck

maybe-installcheck-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-installcheck-polyorb-qos-ada: installcheck-polyorb-qos-ada
installcheck-polyorb-qos-ada: all-polyorb-qos-ada
	r_dir='polyorb-qos-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) installcheck

################################################################################

maybe-uninstall-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-uninstall-polyorb-hi-ada: uninstall-polyorb-hi-ada
uninstall-polyorb-hi-ada:
	r_dir='polyorb-hi-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" uninstall

maybe-uninstall-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-uninstall-polyorb-hi-c: uninstall-polyorb-hi-c
uninstall-polyorb-hi-c:
	r_dir='polyorb-hi-c'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" uninstall

maybe-uninstall-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-uninstall-polyorb-hi-java: uninstall-polyorb-hi-java
uninstall-polyorb-hi-java:
	r_dir='polyorb-hi-java'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" uninstall

maybe-uninstall-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-uninstall-polyorb-qos-ada: uninstall-polyorb-qos-ada
uninstall-polyorb-qos-ada:
	r_dir='polyorb-qos-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" uninstall

################################################################################

maybe-clean-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-clean-polyorb-hi-ada: clean-polyorb-hi-ada
clean-polyorb-hi-ada:
	r_dir='polyorb-hi-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) clean

maybe-clean-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-clean-polyorb-hi-c: clean-polyorb-hi-c
clean-polyorb-hi-c:
	r_dir='polyorb-hi-c'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) clean

maybe-clean-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-clean-polyorb-hi-java: clean-polyorb-hi-java
clean-polyorb-hi-java:
	r_dir='polyorb-hi-java'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) clean

maybe-clean-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-clean-polyorb-qos-ada: clean-polyorb-qos-ada
clean-polyorb-qos-ada:
	r_dir='polyorb-qos-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) clean

################################################################################

maybe-distclean-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-distclean-polyorb-hi-ada: distclean-polyorb-hi-ada
distclean-polyorb-hi-ada:
	r_dir='polyorb-hi-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distclean

maybe-distclean-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-distclean-polyorb-hi-c: distclean-polyorb-hi-c
distclean-polyorb-hi-c:
	r_dir='polyorb-hi-c'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distclean

maybe-distclean-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-distclean-polyorb-hi-java: distclean-polyorb-hi-java
distclean-polyorb-hi-java:
	r_dir='polyorb-hi-java'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distclean

maybe-distclean-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-distclean-polyorb-qos-ada: distclean-polyorb-qos-ada
distclean-polyorb-qos-ada:
	r_dir='polyorb-qos-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distclean

################################################################################

maybe-check-pok:
@POK_TRUE@maybe-check-pok: check-pok
check-pok: configure-pok
	r_dir='pok'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) tests

maybe-check-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-check-polyorb-hi-ada: check-polyorb-hi-ada
check-polyorb-hi-ada:
	r_dir='polyorb-hi-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && PATH="$(DESTDIR)$(bindir):$$PATH" $(MAKE) check

maybe-check-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-check-polyorb-hi-c: check-polyorb-hi-c
check-polyorb-hi-c:
	r_dir='polyorb-hi-c'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && PATH="$(DESTDIR)$(bindir):$$PATH" $(MAKE) check

maybe-check-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-check-polyorb-hi-java: check-polyorb-hi-java
check-polyorb-hi-java:
	r_dir='polyorb-hi-java'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && PATH="$(DESTDIR)$(bindir):$$PATH" $(MAKE) check

maybe-check-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-check-polyorb-qos-ada: check-polyorb-qos-ada
check-polyorb-qos-ada:
	r_dir='polyorb-qos-ada'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && PATH="$(DESTDIR)$(bindir):$$PATH" $(MAKE) check

################################################################################

maybe-dist-polyorb-hi-ada:
@PO_HI_ADA_TRUE@maybe-dist-polyorb-hi-ada: dist-polyorb-hi-ada
dist-polyorb-hi-ada: configure-polyorb-hi-ada
	r_dir='polyorb-hi-ada'; \
	d_dir='polyorb-hi-ada-dist'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distdir "distdir=$${d_dir}" || exit 1; \
	cd .. && chmod -R go=u-w $${r_dir}/$${d_dir} || exit 1; \
	$(mkdir_p) $(distdir)/$${r_dir} || exit 1; \
	cp -pR $${r_dir}/$${d_dir}/* $(distdir)/$${r_dir}  || exit 1; \
	rm -rf $${r_dir}/$${d_dir}

maybe-dist-polyorb-hi-c:
@PO_HI_C_TRUE@maybe-dist-polyorb-hi-c: dist-polyorb-hi-c
dist-polyorb-hi-c: configure-polyorb-hi-c
	r_dir='polyorb-hi-c'; \
	d_dir='polyorb-hi-c-dist'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distdir "distdir=$${d_dir}" || exit 1; \
	cd .. && chmod -R go=u-w $${r_dir}/$${d_dir} || exit 1; \
	$(mkdir_p) $(distdir)/$${r_dir} || exit 1; \
	cp -pR $${r_dir}/$${d_dir}/* $(distdir)/$${r_dir}  || exit 1; \
	rm -rf $${r_dir}/$${d_dir}

maybe-dist-polyorb-hi-java:
@PO_HI_JAVA_TRUE@maybe-dist-polyorb-hi-java: dist-polyorb-hi-java
dist-polyorb-hi-java: configure-polyorb-hi-java
	r_dir='polyorb-hi-java'; \
	d_dir='polyorb-hi-java-dist'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distdir "distdir=$${d_dir}" || exit 1; \
	cd .. && chmod -R go=u-w $${r_dir}/$${d_dir} || exit 1; \
	$(mkdir_p) $(distdir)/$${r_dir} || exit 1; \
	cp -pR $${r_dir}/$${d_dir}/* $(distdir)/$${r_dir}  || exit 1; \
	rm -rf $${r_dir}/$${d_dir}

maybe-dist-polyorb-qos-ada:
@PO_QOS_ADA_TRUE@maybe-dist-polyorb-qos-ada: dist-polyorb-qos-ada
dist-polyorb-qos-ada: configure-polyorb-qos-ada
	r_dir='polyorb-qos-ada'; \
	d_dir='polyorb-qos-ada-dist'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distdir "distdir=$${d_dir}" || exit 1; \
	cd .. && chmod -R go=u-w $${r_dir}/$${d_dir} || exit 1; \
	$(mkdir_p) $(distdir)/$${r_dir} || exit 1; \
	cp -pR $${r_dir}/$${d_dir}/* $(distdir)/$${r_dir}  || exit 1; \
	rm -rf $${r_dir}/$${d_dir}

################################################################################
# Soft dependenciy against runtimes

all-local: \
	maybe-all-polyorb-hi-ada \
	maybe-all-polyorb-hi-c \
	maybe-all-polyorb-hi-java \
	maybe-all-polyorb-qos-ada

dvi: \
	maybe-dvi-polyorb-hi-ada \
	maybe-dvi-polyorb-hi-c \
	maybe-dvi-polyorb-hi-java \
	maybe-dvi-polyorb-qos-ada

install-data-local: \
	maybe-install-polyorb-hi-ada \
	maybe-install-polyorb-hi-c \
	maybe-install-polyorb-hi-java \
	maybe-install-polyorb-qos-ada

installcheck: \
	maybe-installcheck-polyorb-hi-ada \
	maybe-installcheck-polyorb-hi-c \
	maybe-installcheck-polyorb-hi-java \
	maybe-installcheck-polyorb-qos-ada

uninstall-local: \
	maybe-uninstall-polyorb-hi-ada \
	maybe-uninstall-polyorb-hi-c \
	maybe-uninstall-polyorb-hi-java \
	maybe-uninstall-polyorb-qos-ada

clean-local: \
	maybe-clean-polyorb-hi-ada \
	maybe-clean-polyorb-hi-c \
	maybe-clean-polyorb-hi-java \
	maybe-clean-polyorb-qos-ada

distclean-local: \
	maybe-distclean-polyorb-hi-ada \
	maybe-distclean-polyorb-hi-c \
	maybe-distclean-polyorb-hi-java \
	maybe-distclean-polyorb-qos-ada

maybe-check-runtimes: \
	maybe-check-pok \
	maybe-check-polyorb-hi-ada \
	maybe-check-polyorb-hi-c \
	maybe-check-polyorb-hi-java \
	maybe-check-polyorb-qos-ada

dist-hook: \
	maybe-dist-polyorb-hi-ada \
	maybe-dist-polyorb-hi-c \
	maybe-dist-polyorb-hi-java \
	maybe-dist-polyorb-qos-ada
