
.. _installation:

============
Installation
============

Supported platforms
###################

Ocarina has been compiled and successfully tested on the following platforms:

* GNU/Linux
* Mac OS X
* Windows

.. note:: Ocarina should compile and run on every target for which GNAT is available.


Build requirements
##################

An Ada compiler:

* GNAT Pro, GNAT GPL or FSF/GCC with Ada back-end

.. note:: per construction, the macro configure used to find your GNAT
   compiler looks first to the executable gnatgcc, then adagcc and
   finally to gcc to find out which Ada compiler to use. You should be
   very careful with your path and binaries if you have multiple GNAT
   versions installed. See below explanations on the ADA environment
   variable if you need to override the default guess.

.. note:: Ocarina requires at least GCC/FSF 5.1 or GNAT GPL 2015 to be compiled.

Optional components:

* GNATColl for the Ocarina Python bindings
* Sphinx and the sphinx-bootstrap-theme to build the documentation,
  and a full valid LaTeX installation
* Bound-T for the WCET analysis (:code:`bound_t` backend)
* Cheddar for scheduling analysis (:code:`cheddar` backend)
* MAST for scheduling analysis (:code:`mast` backend)
* RTOS supported by one of the Ocarina runtimes

Build instructions
##################

To compile and install Ocarina, execute in a shell::

       % ./configure [some options]
       % make              (or gmake if your make is not GNU make)
       % make install      (ditto)

This will install files in standard locations. If you want to choose
another prefix than `/usr/local`, give configure use `--prefix` argument

.. note:: you MUST use GNU make to compile this software.

.. note:: If you modify source files, build Ocarina after a checkout
   or make distclean, or the directory hierarchy of the source files,
   you should re-generate autoconf and automake files (configure,
   Makefile.in...); to do this, from the main directory, run::

   ./support/reconfig

Build options
#############

Available options for the configure script include:

* `--enable-doc`: to build the documentation

.. note:: You must first install Sphinx and the sphinx-bootstrap-theme

* `--enable-shared`: to build shared libraries

* `--enable-debug`: enable debugging information generation and
  supplementary runtime checks. Note that this option has a
  significant space and time cost, and is not recommended for
  production use.

* `--enable-python`: to build the Python bindings.

.. note:: This option requires GNATColl to be installed, and Ocarina
  built with shared libraries support.

* `--with-ocarina-runtimes=x`: enable building Ocarina along with the
  requested runtimes. x is a set of valid runtimes located in the
  resources/runtimes directory. x is case insensitive. Examples of
  use:

* `--with-ocarina-runtimes=all`: compile Ocarina along with all the
  runtimes. All the Ocarina runtimes MUST be located in the
  resources/runtimes directory.

* `--with-ocarina-runtimes="polyorb-hi-c PolyORB-HI-Ada"`: compile
  Ocarina along with the PolyORB-HI-Ada and the PolyORB-HI-C
  runtimes.

.. note:: The runtime directories (e.g. :file:`polyorb-hi-ada` or
  :file:`polyorb-hi-c` MUST exist in the resources/runtimes directory.

No option: compile Ocarina along with all the runtimes found in the
resources/runtimes directory.

For more details on available options, one may use the `--help` flag.

The following environment variables can be used to override
configure's guess at what compilers to use:

* `CC`: the C compiler
* `ADA`: the Ada 95 compiler (e.g. gcc, gnatgcc or adagcc)

For example, if you have two versions of GNAT installed and available
in your PATH, and configure picks the wrong one, you can indicate what
compiler should be used with the following syntax::

       % ADA=/path/to/good/compiler/gcc ./configure [options]

Ocarina will be compiled with GNAT build host's configuration,
including run-time library. You may override this setting using
`ADA_INCLUDE_PATH` and `ADA_OBJECTS_PATH` environment variables. See GNAT
User's Guide for more details.

.. note:: Developers building Ocarina from the version control
   repository who need to rebuild the configure and Makefile.in files
   should use the script support/reconfig for this purpose. This
   should be done after each update from the repository. In addition
   to the requirements above, they will need autoconf 2.57 or newer,
   automake 1.6.3 or newer.