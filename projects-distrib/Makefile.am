INTERNAL_PROJECTS = \
	$(srcdir)/ocarina/ocarina_core.gpr.in \
	$(srcdir)/ocarina/ocarina_frontends.gpr \
	$(srcdir)/ocarina/ocarina_backends.gpr \
	$(srcdir)/ocarina/ocarina_transfo.gpr \
	$(srcdir)/ocarina/ocarina_config.gpr

EXTRA_DIST = ocarina.gpr $(INTERNAL_PROJECTS)

PROJECT_DIR = $(libdir)/gnat

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(PROJECT_DIR)
	$(INSTALL) -m 444 $(srcdir)/ocarina.gpr $(DESTDIR)$(PROJECT_DIR)
	$(INSTALL) -d $(DESTDIR)$(PROJECT_DIR)/ocarina
	for f in $(INTERNAL_PROJECTS); do \
	  $(INSTALL) -m 444 $$f $(DESTDIR)$(PROJECT_DIR)/ocarina; \
	done

uninstall-local:
	rm -rf $(DESTDIR)$(PROJECT_DIR)/ocarina.gpr
	rm -rf $(DESTDIR)$(PROJECT_DIR)/ocarina
