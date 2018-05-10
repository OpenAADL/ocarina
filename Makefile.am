SUBDIRS = support tools projects src resources projects-distrib examples doc

AUTOMAKE_OPTIONS = no-dependencies
ACLOCAL_AMFLAGS = -I support
CLEANFILES = config-stamp

EXTRA_DIST = $(top_srcdir)/Makefile.common

# Default testing behavior. Can be overridden by the user
MAKE_TESTS        = yes
CLEAR_AT_TEST_END = yes

test-suite: test-suite-internal

test-suite-internal:
	@if test '$(MAKE_TESTS)' = yes; then \
	  if test -x '$(top_srcdir)/autotest.sh'; then \
	    PATH="$(DESTDIR)$(bindir):$$PATH" \
	    && path_conv="@CYGPATH_W@" path_sep="@PATH_SEP@" \
	       $(top_srcdir)/autotest.sh --tests \
	    && (test '$(CLEAR_AT_TEST_END)' = yes \
		&& clear \
		&& echo "TESTSUITE SUCCESSFULLY PASSED" \
		&& echo "TESTED: `which ocarina`" \
		&& sleep 5 || true) || exit 1;\
	  fi; \
	  if test -x '$(top_srcdir)/../autotest.sh'; then \
	    PATH="$(DESTDIR)$(bindir):$$PATH" \
	    && path_conv="@CYGPATH_W@" path_sep="@PATH_SEP@" \
	       $(top_srcdir)/../autotest.sh --tests \
	    && (test '$(CLEAR_AT_TEST_END)' = yes \
		&& clear \
		&& echo "TESTSUITE SUCCESSFULLY PASSED" \
		&& echo "TESTED: `which ocarina`"\
		&& sleep 5 || true) || exit 1;\
	  fi; \
	fi

tests::
	@$(MAKE) test-suite-internal "MAKE_TESTS=yes" "CLEAR_AT_TEST_END=no"

examples::
	@$(MAKE) -C resources/runtime maybe-check-runtimes

gnatcheck::
	@$(MAKE) -C projects gnatcheck

gnatmetric::
	@$(MAKE) -C projects gnatmetric

check-local: install
	@$(MAKE) test-suite
	@$(MAKE) -C resources/runtime maybe-check-runtimes

STRIP = "@abs_top_srcdir@/support/strip_wrapper.sh"

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL) -d $(DESTDIR)$(datadir)
	$(INSTALL) -d $(DESTDIR)$(datadir)/doc
	$(INSTALL) -d $(DESTDIR)$(datadir)/doc/ocarina
	$(INSTALL_SCRIPT) ocarina-config $(DESTDIR)$(bindir)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/ocarina-config

release:
	rm -rf release/
	mkdir release/
	$(MAKE) dist
	cp -f $(DIST_ARCHIVES) release/$(DIST_ARCHIVES)

# We disable checking of the distclean rule, it is a nuisance
distcleancheck:
	@:
