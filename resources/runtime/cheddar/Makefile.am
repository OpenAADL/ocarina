AUTOMAKE_OPTIONS = no-dependencies

CHEDDAR_FILES = $(srcdir)/cheddar.dtd

EXTRA_DIST = $(CHEDDAR_FILES)

cheddar_dtd = ${shell $(CYGPATH_U) '$(includedir)/ocarina/runtime/cheddar'}

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(cheddar_dtd)
	for f in $(CHEDDAR_FILES); do $(INSTALL) -m 444 $$f $(DESTDIR)$(cheddar_dtd) ; done
