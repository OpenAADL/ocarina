AUTOMAKE_OPTIONS = no-dependencies

PYTHON_FILES = $(srcdir)/ocarina/ocarina.py				\
	$(srcdir)/ocarina/__init.py__					\
	$(srcdir)/ocarina/ocarina_common_tools.py $(srcdir)/setup.py

if INSTALL_PYTHON
PYTHON_FILES += \
	 $(top_builddir)/src/python/ocarina_me_aadl_aadl_instances_nodes.py \
	 $(top_builddir)/src/python/ocarina_me_aadl_aadl_tree_nodes.py
endif

EXTRA_DIST = $(PYTHON_FILES)

ocarina_python = ${shell $(CYGPATH_U) '$(includedir)/ocarina/runtime/python'}

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(ocarina_python)
	pip install --user .
