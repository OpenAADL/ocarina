AUTOMAKE_OPTIONS = no-dependencies

PYTHON_FILES = $(srcdir)/ocarina.py

EXTRA_DIST = $(PYTHON_FILES)

ocarina_python = ${shell $(CYGPATH_U) '$(includedir)/ocarina/runtime/python'}

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(ocarina_python)
	for f in $(PYTHON_FILES); do $(INSTALL) -m 444 $$f $(DESTDIR)$(ocarina_python) ; done
