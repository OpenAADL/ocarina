SUBDIRS = runtime behavioural_properties

AADL_COMMON_PROPERTIES = $(srcdir)/ocarina_config.aadl \
	$(srcdir)/assert_properties_extended.aadl \
	$(srcdir)/data_model.aadl \
	$(srcdir)/deployment.aadl

AADL_V1_PROPERTIES = $(srcdir)/AADLv1/aadl_properties.aadl \
	$(srcdir)/AADLv1/assert_types.aadl \
	$(srcdir)/AADLv1/assert_properties.aadl \
	$(srcdir)/AADLv1/Cheddar_Properties.aadl \
	$(srcdir)/AADLv1/aadl_project.aadl \
	$(srcdir)/AADLv1/base_types.aadl

AADL_V2_PROPERTIES = $(srcdir)/AADLv2/aadl_project.aadl		\
	$(srcdir)/AADLv2/arinc653.aadl				\
	$(srcdir)/AADLv2/assert_types.aadl			\
	$(srcdir)/AADLv2/assert_properties.aadl			\
	$(srcdir)/AADLv2/behavior_properties.aadl		\
	$(srcdir)/AADLv2/deployment_properties.aadl		\
	$(srcdir)/AADLv2/emv2.aadl				\
	$(srcdir)/AADLv2/Cheddar_Properties.aadl		\
	$(srcdir)/AADLv2/thread_properties.aadl			\
	$(srcdir)/AADLv2/communication_properties.aadl		\
	$(srcdir)/AADLv2/memory_properties.aadl			\
	$(srcdir)/AADLv2/modeling_properties.aadl		\
	$(srcdir)/AADLv2/timing_properties.aadl			\
	$(srcdir)/AADLv2/pok_properties.aadl			\
	$(srcdir)/AADLv2/programming_properties.aadl		\
	$(srcdir)/AADLv2/base_types.aadl			\
	$(srcdir)/AADLv2/replication_properties.aadl		\
	$(srcdir)/AADLv2/errorlibrary.aadl			\
	$(srcdir)/AADLv2/transformations.aadl			\
	$(srcdir)/AADLv2/ocarina_library.aadl			\
	$(srcdir)/AADLv2/fmi.aadl				\
	$(srcdir)/AADLv2/code_generation_properties.aadl

AADL_V2_COMPONENTS = $(srcdir)/AADLv2/components/*.aadl \
		     $(srcdir)/AADLv2/components/devices/*.aadl \
		     $(srcdir)/AADLv2/components/processors/*.aadl

EXTRA_DIST = $(AADL_COMMON_PROPERTIES) \
	$(AADL_V1_PROPERTIES) \
	$(AADL_V2_PROPERTIES) \
	$(AADL_V2_COMPONENTS)

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(datadir)/ocarina
	$(INSTALL) -d $(DESTDIR)$(datadir)/ocarina/AADLv1
	$(INSTALL) -d $(DESTDIR)$(datadir)/ocarina/AADLv2
	for file in $(AADL_COMMON_PROPERTIES); do \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(datadir)/ocarina/AADLv1 || exit 1; \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(datadir)/ocarina/AADLv2 || exit 1; \
	done
	for file in $(AADL_V2_PROPERTIES) $(AADL_V2_COMPONENTS); do \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(datadir)/ocarina/AADLv2 || exit 1; \
	done
	for file in $(AADL_V1_PROPERTIES); do \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(datadir)/ocarina/AADLv1 || exit 1; \
	done

build_ocarina_components:
	rm -f $(DESTDIR)$(datadir)/ocarina/AADLv2/ocarina_components.aadl
	for file in $(srcdir)/AADLv2/components/*.aadl; do \
	  cat $$file >> $(DESTDIR)$(datadir)/ocarina/AADLv2/ocarina_components.aadl || exit 1; \
	done
	for file in $(srcdir)/AADLv2/components/devices/*.aadl; do \
	  cat $$file >> $(DESTDIR)$(datadir)/ocarina/AADLv2/ocarina_components.aadl || exit 1; \
	done
	for file in $(srcdir)/AADLv2/components/processors/*.aadl; do \
	  cat $$file >> $(DESTDIR)$(datadir)/ocarina/AADLv2/ocarina_components.aadl || exit 1; \
	done
	echo "s/OCARINA_INCLUDE_PATH/"$(shell echo $(DESTDIR)$(includedir)|$(SED) -e 's/\//\\\\\//g')"/g" > sedscript
	$(SED) -f sedscript $(DESTDIR)$(datadir)/ocarina/AADLv2/ocarina_components.aadl > tmp.aadl
	mv -f tmp.aadl $(DESTDIR)$(datadir)/ocarina/AADLv2/ocarina_components.aadl

uninstall-local:
	rm -rf $(DESTDIR)$(datadir)/ocarina
