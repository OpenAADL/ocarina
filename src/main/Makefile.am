include $(top_srcdir)/Makefile.common

EXTRA_DIST = ${filter-out $(srcdir)/ocarina-configuration.adb, $(ADA_SPECS) ${ADA_BODIES}}

STRIP = "@abs_top_srcdir@/support/strip_wrapper.sh"

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM_ENV) $(INSTALL_PROGRAM) ocarina$(EXE_SUFFIX) \
	   $(DESTDIR)$(bindir)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ocarina$(EXE_SUFFIX)

install-data-local: install-ada-libs
	$(INSTALL) -d $(DESTDIR)$(adadir)
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(ADA_SPECS) $(ADA_BODIES); do \
	  $(INSTALL) -m 444 $$f $(DESTDIR)$(adadir); \
	done
	for f in objects/*.ali; do \
	  $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); \
	done
	$(INSTALL) -d $(DESTDIR)$(libdir)
	if [ "$(shell ls -A libs/lib*)" ]; then \
	for f in libs/lib*; do \
	  $(INSTALL) -m 444 $$f $(DESTDIR)$(libdir); \
	done \
	fi

# When packaging, we remove the "Working Copy from @SVN_REVISION@"
# from the SVN_Revision variable in ocarina-configuration.adb.in and
# we hard-code the latest SVN revision.

Sed_Pattern = 's/\(SVN_Revision *: *constant *String *:= *\)"Working *Copy *from.*";/\1"$(SVN_REVISION)";/'

dist-hook:
	@echo "Setting the SVN revision"
	cat $(distdir)/ocarina-configuration.adb.in | sed -e $(Sed_Pattern) \
	   > $(distdir)/ocarina-configuration.adb.in.new
	mv -f $(distdir)/ocarina-configuration.adb.in.new \
	   $(distdir)/ocarina-configuration.adb.in
