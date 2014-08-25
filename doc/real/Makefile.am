LATEX_FILE=real

REAL_CODE=$(srcdir)/all_pcp.real $(srcdir)/connections.real	\
$(srcdir)/latency.real $(srcdir)/pcp.real $(srcdir)/rma.real	\
$(srcdir)/arinc_secure.real $(srcdir)/independant_flows.real	\
$(srcdir)/memory_size.real periodic_tasks.real			\
$(srcdir)/stack_size.real $(srcdir)/bus_rate.real		\
$(srcdir)/independant_threads.real $(srcdir)/mutexes.real	\
$(srcdir)/processes.real $(srcdir)/unique_call.real

texinputs:=$(srcdir):$(TEXINPUTS)
export TEXINPUTS:=$(texinputs)
bibinputs:=$(srcdir):$(BIBINPUTS)
export BIBINPUTS:=$(bibinputs)

all-local:

build: $(LATEX_FILE).tex biblio.bib $(REAL_CODE)
	latex $(srcdir)/$(LATEX_FILE).tex ;
	bibtex $(LATEX_FILE) ;
	latex $(srcdir)/$(LATEX_FILE).tex ;
	latex $(srcdir)/$(LATEX_FILE).tex ;
	dvipdf $(LATEX_FILE).dvi

$(REAL_CODE):
	for file in $(REAL_CODE); do \
	  cp -f $(top_srcdir)/examples/real/lib/`basename $${file}` $(srcdir)/; \
	done

clean-local:
	rm -f *~ *.dvi *.log *.aux *.out  *.toc *.bbl *.blg $(REAL_CODE)

distclean-local: clean
	rm -f $(LATEX_FILE).pdf
	rm -rf sources

install-data-local: all
	$(INSTALL) -d $(DESTDIR)$(datadir)/doc/ocarina
	-$(INSTALL_DATA) $(LATEX_FILE).pdf $(DESTDIR)$(datadir)/doc/ocarina

EXTRA_DIST= $(srcdir)/sources2/range_set.real				\
$(srcdir)/sources2/evaluation.real $(srcdir)/sources2/data.aadl		\
$(srcdir)/sources2/embedded_real.aadl					\
$(srcdir)/sources2/set_building.real $(srcdir)/sources2/data_def.aadl	\
$(srcdir)/variables.tex $(srcdir)/real.tex $(srcdir)/bnf.sty		\
$(srcdir)/check_functions.tex $(srcdir)/keywords.tex			\
$(srcdir)/sets.tex $(srcdir)/aadl_definition.tex			\
$(srcdir)/figs/figure-aadl-v4.eps $(srcdir)/biblio.bib			\
$(srcdir)/theorems.tex $(srcdir)/selection_functions.tex		\
$(srcdir)/predefined.tex $(srcdir)/real_bnf.tex				\
$(srcdir)/expressions.tex $(srcdir)/examples.tex			\
$(srcdir)/real_definition.tex $(srcdir)/aadl_resources.tex		\
$(srcdir)/types.tex $(srcdir)/conclusion.tex $(REAL_CODE)
