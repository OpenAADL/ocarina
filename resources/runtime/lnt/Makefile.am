AUTOMAKE_OPTIONS = no-dependencies

LNT_FILES = $(srcdir)/LNT_Generic_Process_For_Port_Connections.lnt	\
	$(srcdir)/LNT_Generic_Process_For_Port_Connections_BA.lnt

EXTRA_DIST = $(LNT_FILES)

lnt_generic = ${shell $(CYGPATH_U) '$(includedir)/ocarina/runtime/lnt'}

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(lnt_generic)
	for f in $(LNT_FILES); do $(INSTALL) -m 444 $$f $(DESTDIR)$(lnt_generic) ; done
