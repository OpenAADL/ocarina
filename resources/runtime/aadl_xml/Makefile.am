AUTOMAKE_OPTIONS = no-dependencies

AADL_XML_FILES = $(srcdir)/aadl.xsd

EXTRA_DIST = $(AADL_XML_FILES)

aadl_xml_xsd = ${shell $(CYGPATH_U) '$(includedir)/ocarina/runtime/aadl_xml'}

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(aadl_xml_xsd)
	for f in $(AADL_XML_FILES); do $(INSTALL) -m 444 $$f $(DESTDIR)$(aadl_xml_xsd) ; done
