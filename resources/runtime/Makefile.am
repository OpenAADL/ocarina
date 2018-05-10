SUBDIRS = cheddar aadl_xml python alloy lnt

@DEBUG_FALSE@DEBUG_FLAG = --disable-debug
@DEBUG_TRUE@DEBUG_FLAG = --enable-debug

################################################################################

maybe-reconfig-aadlib:
@AADLIB_TRUE@maybe-reconfig-aadlib: reconfig-aadlib
reconfig-aadlib:
	r_dir='aadlib'; \
	test -f $(srcdir)/$${r_dir}/configure && exit 0 || true; \
	test -d $(srcdir)/$${r_dir} || exit 0; \
	cd $(srcdir)/$${r_dir} && ./support/reconfig

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

################################################################################

maybe-configure-pok:
@POK_TRUE@maybe-configure-pok: configure-pok
configure-pok:
	r_dir='pok'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) configure

maybe-configure-aadlib:
@AADLIB_TRUE@maybe-configure-aadlib: configure-aadlib
configure-aadlib: reconfig-aadlib
	r_dir='aadlib'; \
	s_dir=`(cd $(srcdir) && pwd)`; \
	test -f $${r_dir}/Makefile && exit 0 || true; \
	test -x $(srcdir)/$${r_dir}/configure || exit 0; \
	$(mkdir_p) $${r_dir} && cd $${r_dir} || exit 1; \
	$${s_dir}/$${r_dir}/configure $(DEBUG_FLAG) $(DOC_FLAG) \
	   --with-ocarina-prefix='$(prefix)' --disable-ocarina-version-check

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

################################################################################

maybe-all-aadlib:
@AADLIB_TRUE@maybe-all-aadlib: all-aadlib
all-aadlib: configure-aadlib
	r_dir='aadlib'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) all

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

################################################################################

maybe-install-aadlib:
@AADLIB_TRUE@maybe-install-aadlib: install-aadlib
install-aadlib: all-aadlib
	r_dir='aadlib'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" install

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

################################################################################

maybe-installcheck-aadlib:
@AADLIB_TRUE@maybe-installcheck-aadlib: installcheck-aadlib
installcheck-aadlib: all-aadlib
	r_dir='aadlib'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) installcheck

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

################################################################################

maybe-uninstall-aadlib:
@AADLIB_TRUE@maybe-uninstall-aadlib: uninstall-aadlib
uninstall-aadlib:
	r_dir='aadlib'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) DESTDIR="$(DESTDIR)" uninstall

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


################################################################################

maybe-clean-aadlib:
@AADLIB_TRUE@maybe-clean-aadlib: clean-aadlib
clean-aadlib:
	r_dir='aadlib'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) clean

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

################################################################################

maybe-distclean-aadlib:
@AADLIB_TRUE@maybe-distclean-aadlib: distclean-aadlib
distclean-aadlib:
	r_dir='aadlib'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distclean

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

################################################################################

maybe-check-pok:
@POK_TRUE@maybe-check-pok: check-pok
check-pok: configure-pok
	r_dir='pok'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) tests

maybe-check-aadlib:
@AADLIB_TRUE@maybe-check-aadlib: check-aadlib
check-aadlib:
	r_dir='aadlib'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && PATH="$(DESTDIR)$(bindir):$$PATH" $(MAKE) check

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

################################################################################

maybe-dist-aadlib:
@AADLIB_TRUE@maybe-dist-aadlib: dist-aadlib
dist-aadlib: configure-aadlib
	r_dir='aadlib'; \
	d_dir='aadlib-dist'; \
	test -f $${r_dir}/Makefile || exit 0; \
	cd $${r_dir} && $(MAKE) distdir "distdir=$${d_dir}" || exit 1; \
	cd .. && chmod -R go=u-w $${r_dir}/$${d_dir} || exit 1; \
	$(mkdir_p) $(distdir)/$${r_dir} || exit 1; \
	cp -pR $${r_dir}/$${d_dir}/* $(distdir)/$${r_dir}  || exit 1; \
	rm -rf $${r_dir}/$${d_dir}

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

################################################################################
# Soft dependenciy against runtimes

all-local: \
	maybe-all-aadlib \
	maybe-all-polyorb-hi-ada \
	maybe-all-polyorb-hi-c

dvi: \
	maybe-dvi-polyorb-hi-ada \
	maybe-dvi-polyorb-hi-c

install-data-local: \
	maybe-install-aadlib \
	maybe-install-polyorb-hi-ada \
	maybe-install-polyorb-hi-c

installcheck: \
	maybe-installcheck-aadlib \
	maybe-installcheck-polyorb-hi-ada \
	maybe-installcheck-polyorb-hi-c

uninstall-local: \
	maybe-uninstall-aadlib \
	maybe-uninstall-polyorb-hi-ada \
	maybe-uninstall-polyorb-hi-c

clean-local: \
	maybe-clean-aadlib \
	maybe-clean-polyorb-hi-ada \
	maybe-clean-polyorb-hi-c

distclean-local: \
	maybe-distclean-aadlib \
	maybe-distclean-polyorb-hi-ada \
	maybe-distclean-polyorb-hi-c

maybe-check-runtimes: \
	maybe-check-pok \
	maybe-check-aadlib \
	maybe-check-polyorb-hi-ada \
	maybe-check-polyorb-hi-c

dist-hook: \
	maybe-dist-aadlib \
	maybe-dist-polyorb-hi-ada \
	maybe-dist-polyorb-hi-c
