USR_DEF_TEMPLATE = user_defined.psc

EXTRA_DIST = $(USR_DEF_TEMPLATE)

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(datadir)/ocarina
	for f in $(USR_DEF_TEMPLATE); do \
	  $(INSTALL_DATA) $(srcdir)/$$f $(DESTDIR)$(datadir)/ocarina/$$f; \
	done

