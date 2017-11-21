SUBDIRS= lib resources safety security

EXTRA_DIST = $(srcdir)/../Makefile.common

SAMPLE_DIR = $(prefix)/examples/ocarina/real

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(SAMPLE_DIR)
	$(INSTALL_DATA) $(srcdir)/README $(DESTDIR)$(SAMPLE_DIR)

uninstall-local:
	rm -rf $(DESTDIR)$(SAMPLE_DIR)
