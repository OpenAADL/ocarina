AUTOMAKE_OPTIONS = no-dependencies

BASE_ALLOY_FILES = contract/analysis/example_scheduling_analysis.als	\
	contract/goal/example_scheduling_goal.als		\
	lib/data_structure.als lib/property.als main.als	\
	meta/contract.als

ALLOY_FILES = $(addprefix $(srcdir)/,$(BASE_ALLOY_FILES))

EXTRA_DIST = $(ALLOY_FILES)

alloy_files = ${shell $(CYGPATH_U) '$(includedir)/ocarina/runtime/alloy'}

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alloy_files)
	for f in $(BASE_ALLOY_FILES); do $(INSTALL) -D -m 444 $(srcdir)/$$f $(DESTDIR)$(alloy_files)/$$f; done
