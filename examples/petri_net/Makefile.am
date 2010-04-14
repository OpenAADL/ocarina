SUBDIRS= robot

EXTRA_DIST = $(srcdir)/../Makefile.common

SAMPLE_DIR = $(prefix)/examples/ocarina/petri_net

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(SAMPLE_DIR)
	$(INSTALL_DATA) $(srcdir)/README $(DESTDIR)$(SAMPLE_DIR)

uninstall-local:
	rm -rf $(DESTDIR)$(SAMPLE_DIR)
