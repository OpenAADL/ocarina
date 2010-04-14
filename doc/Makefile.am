# For now, do not build real documentation, it requires LaTeX files
# that may not be installed. However, its source is included in the
# packaging.
DIST_SUBDIRS = real

ADAFILES=  $(srcdir)/sources/parse_and_print_aadl.adb

AADLFILES= $(srcdir)/sources/scenario_ada.aadl			\
	$(srcdir)/sources/scenario_c.aadl			\
	$(srcdir)/sources/scenario_common.aadl			\
	$(srcdir)/sources/example_pn.aadl			\
	$(top_srcdir)/resources/AADLv1/aadl_project.aadl	\
	$(top_srcdir)/resources/AADLv1/aadl_properties.aadl	\
	$(top_srcdir)/resources/deployment.aadl			\
	$(top_srcdir)/resources/ocarina_config.aadl

APIFILES=								      \
$(top_srcdir)/src/core/tree/ocarina-me_aadl-aadl_tree-entities.ads	      \
$(top_srcdir)/src/core/tree/ocarina-me_aadl-aadl_tree-entities-properties.ads \
$(top_srcdir)/src/core/model/ocarina-builder.ads			      \
$(top_srcdir)/src/core/model/ocarina-builder-aadl.ads                         \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-annexes.ads		      \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-components.ads	      \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-components-connections.ads  \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-components-features.ads     \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-components-flows.ads	      \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-components-modes.ads	      \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-components-subcomponents.ads\
$(top_srcdir)/src/core/model/ocarina-builder-aadl-components-subprogram_calls.ads  \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-namespaces.ads              \
$(top_srcdir)/src/core/model/ocarina-builder-aadl-properties.ads	      \
$(top_srcdir)/src/core/model/ocarina-analyzer-aadl-finder.ads		      \
$(top_srcdir)/src/core/model/ocarina-analyzer-aadl-queries.ads

GEN_TEXIFILES= ${notdir $(ADAFILES:=.texi) $(AADLFILES:=.texi) $(APIFILES:=.texi)}

IMAGE_NAMES=\
	$(srcdir)/images/component\
	$(srcdir)/images/connection\
	$(srcdir)/images/feature\
	$(srcdir)/images/mubroker_complet\
	$(srcdir)/images/spg_connection\
	$(srcdir)/images/subprogram

ALL_IMAGES= $(IMAGE_NAMES:=.eps) $(IMAGE_NAMES:=.png) $(IMAGE_NAMES:=.pdf)

KW_FILES = \
	$(srcdir)/aadl.kw\
	$(srcdir)/ada.kw

SED_FILES = ${notdir $(KW_FILES:.kw=.sed)}

GUIDE=ocarina

GUIDE_TEXIFILES =\
	$(srcdir)/$(GUIDE).texi\
	$(GEN_TEXIFILES) \
	gfdl.texi

EXTRA_DIST=\
	$(srcdir)/CODING_GUIDELINES \
	$(srcdir)/CONTRIBUTING \
	$(srcdir)/PROBLEM-REPORT-FORM \
	$(GUIDE_TEXIFILES)\
	$(ALL_IMAGES)\
	$(ADAFILES)\
	$(AADLFILES)\
	$(KW_FILES)\
	$(srcdir)/ocarina.css

info_TEXINFOS = ocarina.texi
ocarina_TEXINFOS=$(GUIDE_TEXIFILES)

all-local: $(GUIDE).html $(GUIDE).pdf

$(GUIDE).html: $(IMAGES) $(GUIDE_TEXIFILES)
	$(MAKEINFO) --css-include=$(srcdir)/ocarina.css --html $(srcdir)/$(GUIDE).texi -o $(GUIDE).html
	rm -rf $(GUIDE).html/images
	mkdir -p $(GUIDE).html/images
	cp -p $(srcdir)/images/*.png $(GUIDE).html/images/
	INDEX_FILE="$(GUIDE).html/index.html";\
	TMP_FILE="$$INDEX_FILE.tmp"; \
	cat "$$INDEX_FILE" | sed 's/$$LastChangedDate: \([^$$]*\) \$$/Last Modified: \1/g' > "$$TMP_FILE"; \
	mv -f "$$TMP_FILE" "$$INDEX_FILE"

$(GUIDE).pdf: $(IMAGES) $(GUIDE_TEXIFILES)
	$(TEXI2PDF) $(srcdir)/$(GUIDE).texi

clean-local:
	rm -f *.aux *.log *.dvi *.info *.tmp
	rm -f *.cp *.pg *.toc *.vr *.fn *.ky *.tp *~

distclean-local:
	rm -f $(GEN_TEXIFILES)
	rm -rf *-stamp
	rm -f *.sed
	rm -rf $(GUIDE).html $(GUIDE).pdf

# In VPATH builds, make difference between the case the doc is built
# by the user and the case it is provided to him by the package.

dist-hook: all
	if [ -r $(GUIDE).pdf ]; then \
	  cp -p -r $(GUIDE).html $(GUIDE).pdf $(distdir); \
	  cp -p $(SED_FILES) $(distdir); \
	else \
	  cp -r $(srcdir)/$(GUIDE).html $(srcdir)/$(GUIDE).pdf $(distdir); \
	  cp -p ${addprefix $(srcdir)/, $(SED_FILES)} $(distdir); \
	fi
	touch $(distdir)/sed-stamp
	touch $(distdir)/ada-stamp
	touch $(distdir)/adb-stamp
	touch $(distdir)/ads-stamp
	touch $(distdir)/aadl-stamp

install-data-local: all
	$(INSTALL) -d $(DESTDIR)$(datadir)/doc/ocarina
	if [ -r $(GUIDE).pdf ]; then \
	  $(INSTALL_DATA) $(GUIDE).pdf $(DESTDIR)$(datadir)/doc/ocarina; \
	  $(INSTALL) -d $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html; \
	  for f in $(GUIDE).html/*.html; do \
            $(INSTALL_DATA) $$f $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html; \
          done; \
	  $(INSTALL) -d $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html/images; \
	  for f in $(GUIDE).html/images/*.png; do \
            $(INSTALL_DATA) $$f $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html/images; \
          done; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(GUIDE).pdf $(DESTDIR)$(datadir)/doc/ocarina; \
	  $(INSTALL) -d $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html; \
	  for f in $(srcdir)/$(GUIDE).html/*.html; do \
            $(INSTALL_DATA) $$f $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html; \
          done; \
	  $(INSTALL) -d $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html/images; \
	  for f in $(srcdir)/$(GUIDE).html/images/*.png; do \
            $(INSTALL_DATA) $$f $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html/images; \
          done; \
	fi
	$(INSTALL_DATA)	$(srcdir)/CONTRIBUTING $(DESTDIR)$(datadir)/doc/ocarina
	$(INSTALL_DATA)	$(srcdir)/PROBLEM-REPORT-FORM $(DESTDIR)$(datadir)/doc/ocarina

uninstall-local:
	rm -rf $(DESTDIR)$(datadir)/doc/ocarina

$(GUIDE_TEXIFILES): ada-stamp adb-stamp ads-stamp aadl-stamp

ada-stamp: $(top_srcdir)/tools/gentexifile $(ADAFILES) sed-stamp
	for f in ${filter %.ada, $(ADAFILES)}; do \
	  $(SHELL) $(top_srcdir)/tools/gentexifile $$f; \
	done
	touch ada-stamp

adb-stamp: $(top_srcdir)/tools/gentexifile $(ADAFILES) sed-stamp
	for f in ${filter %.adb, $(ADAFILES)}; do \
	  $(SHELL) $(top_srcdir)/tools/gentexifile $$f; \
	done
	touch adb-stamp

ads-stamp: $(top_srcdir)/tools/gen_api_doc $(APIFILES)
	ABS_TOP_SRCDIR="@abs_top_srcdir@" \
	ABS_TOP_BUILDDIR="@abs_top_builddir@" \
	  $(SHELL) $(top_srcdir)/tools/gen_api_doc
	touch ads-stamp

aadl-stamp: $(top_srcdir)/tools/gentexifile $(AADLFILES) sed-stamp
	for f in $(AADLFILES); do \
	  $(SHELL) $(top_srcdir)/tools/gentexifile $$f; \
	done
	touch aadl-stamp

sed-stamp: $(top_srcdir)/tools/gensedfile $(KW_FILES)
	for f in $(KW_FILES); do \
	  $(SHELL) $(top_srcdir)/tools/gensedfile $$f; \
	done
	touch sed-stamp
