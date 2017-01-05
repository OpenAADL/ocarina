SUBDIRS=real

GUIDE=ocarina

man_MANS = ocarina-config.1 ocarina.1

GUIDE_RSTFILES = $(srcdir)/about.rst $(srcdir)/gfdl.rst			\
	$(srcdir)/introduction.rst $(srcdir)/annexes.rst index.rst	\
	$(srcdir)/python.rst $(srcdir)/editors.rst			\
	$(srcdir)/installation.rst $(srcdir)/usage.rst

EXTRA_DIST= $(GUIDE_TEXIFILES)	\
	$(ALL_IMAGES) $(man_MANS) $(srcdir)/ocarina-config.html	\
	 $(srcdir)/ocarina_man.html $(srcdir)/ocarina.css

docs: $(GUIDE).html $(GUIDE).pdf

all-local: @DOC_TARGETS@

$(GUIDE).html: $(GUIDE_RSTFILES)
	make  html

$(GUIDE).pdf: $(IMAGES) $(GUIDE_RSTFILES)
	make  latexpdf

clean-local:
	rm -f *~
	-rm -rf $(BUILDDIR)/*

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
	fi
	if [ -r $(srcdir)/$(GUIDE).pdf ]; then \
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
	if [ -r $(builddir)/_build/latex/$(GUIDE).pdf ]; then \
	  $(INSTALL_DATA) $(builddir)/_build/latex/$(GUIDE).pdf $(DESTDIR)$(datadir)/doc/ocarina; \
	  $(INSTALL) -d $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html; \
	  $(CP) -r $(builddir)/_build/html/* $(DESTDIR)$(datadir)/doc/ocarina/$(GUIDE).html; \
	fi
	$(INSTALL_DATA)	$(srcdir)/ocarina-config.html $(DESTDIR)$(datadir)/doc/ocarina
	$(INSTALL_DATA)	$(srcdir)/ocarina_man.html $(DESTDIR)$(datadir)/doc/ocarina
	$(INSTALL_DATA)	$(srcdir)/ocarina.css $(DESTDIR)$(datadir)/doc/ocarina

uninstall-local:
	rm -rf $(DESTDIR)$(datadir)/doc/ocarina


###############################################################################
# SPHINX rules

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = $(builddir)/_build

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -c $(srcdir) -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) $(srcdir)
# the i18n builder cannot share the environment and doctrees with the others
I18NSPHINXOPTS  = $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

.PHONY: help clean html dirhtml singlehtml pickle json htmlhelp qthelp devhelp epub latex latexpdf text man changes linkcheck doctest gettext

html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."

latexpdf:
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/latex
	@echo "Running LaTeX files through pdflatex..."
	$(MAKE) -C $(BUILDDIR)/latex all-pdf
	@echo "pdflatex finished; the PDF files are in $(BUILDDIR)/latex."

###############################################################################
# Building manpages

build_man: $(top_srcdir)/ocarina-config.in
	chmod 755 $(top_builddir)/ocarina-config
	help2man --no-discard-stderr $(top_builddir)/ocarina-config >  $(top_srcdir)/doc/ocarina-config.1
	echo ".HEAD \"<link rel=\"\"stylesheet\"\" type=\"\"text/css\"\" href=\"\"ocarina.css\"\" >\"" >> $(top_srcdir)/doc/ocarina-config.1
	groff -man -Thtml $(top_srcdir)/doc/ocarina-config.1 > $(top_srcdir)/doc/ocarina-config.html
	-help2man --no-discard-stderr `which ocarina` >  $(top_srcdir)/doc/ocarina.1
	echo ".HEAD \"<link rel=\"\"stylesheet\"\" type=\"\"text/css\"\" href=\"\"ocarina.css\"\" >\"" >> $(top_srcdir)/doc/ocarina.1
	groff -man -Thtml $(top_srcdir)/doc/ocarina.1 > $(top_srcdir)/doc/ocarina_man.html
	groff -man -Thtml $(top_srcdir)/doc/ocarina.1 > $(top_srcdir)/doc/ocarina_man.html
	ocarina -h >  $(top_srcdir)/doc/ocarina_man.txt
	-ocarina-config -h 2>  $(top_srcdir)/doc/ocarina_config.txt
