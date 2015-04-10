AUTOMAKE_OPTIONS = no-dependencies

PYTHON_FILES = $(srcdir)/ocarina.py					   

if INSTALL_PYTHON
PYTHON_FILES + = \
	 $(top_builddir)/src/python/ocarina_me_aadl_aadl_instances_nodes.py \
	 $(top_builddir)/src/python/ocarina_me_aadl_aadl_tree_nodes.py
endif

EXTRA_DIST = $(PYTHON_FILES)

ocarina_python = ${shell $(CYGPATH_U) '$(includedir)/ocarina/runtime/python'}

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(ocarina_python)
	for f in $(PYTHON_FILES); do $(INSTALL) -m 444 $$f $(DESTDIR)$(ocarina_python) ; done
