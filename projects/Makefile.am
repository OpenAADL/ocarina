include $(top_srcdir)/Makefile.common

# The order of the project files is important for saving compile time

BUILD_PROJECTS= ocarina-core.gpr ocarina-frontends.gpr			\
	ocarina-backends.gpr ocarina-config.gpr ocarina-transfo.gpr	\
	ocarina-main.gpr $(PYTHON_PRJ)

# All project files have to be packaged with "make dist"

EXTRA_DIST= $(srcdir)/ocarina.gpr.in $(srcdir)/ocarina-frontends.gpr	\
	$(srcdir)/ocarina-backends.gpr $(srcdir)/ocarina-core.gpr	\
	$(srcdir)/ocarina-main.gpr $(srcdir)/ocarina-transfo.gpr	\
	$(srcdir)/ocarina-config.gpr $(srcdir)/restrictions.adc


BUILD_STAMPS = $(BUILD_PROJECTS:.gpr=.gpr-build)

CLEAN_PROJECTS = $(BUILD_PROJECTS)
CLEAN_STAMPS = $(CLEAN_PROJECTS:.gpr=.gpr-clean)

# Object directories (*.o, *.ali)

OBJECT_DIRS = $(top_builddir)/src/core/objects	\
	$(top_builddir)/src/config/objects	\
	$(top_builddir)/src/frontends/objects	\
	$(top_builddir)/src/backends/objects	\
	$(top_builddir)/src/transfo/objects	\
	$(top_builddir)/src/main/objects	\
	$(top_builddir)/src/python/objects

# Library directories (*.ali, *.a)

LIB_DIRS = $(top_builddir)/src/core/libs				\
	$(top_builddir)/src/frontends/libs				\
	$(top_builddir)/src/backends/libs				\
	$(top_builddir)/src/transfo/libs				\
	$(top_builddir)/src/config/libs $(top_builddir)/src/main/libs	\
	$(top_builddir)/src/python/libs

# All directories used for the build process. If the configure is
# performed from a directory different from the top source directory,
# AND ONLY IN THIS CASE, we need to create some additional
# subdirectories which are used in project files

EXTRA_BUILD_DIRS = /src/core/tree

BUILD_DIRS = ${filter-out \
      ${addprefix $(top_srcdir), $(EXTRA_BUILD_DIRS)}, \
      $(OBJECT_DIRS) $(LIB_DIRS) ${addprefix $(top_builddir), $(EXTRA_BUILD_DIRS)}}

BUILD_DIRS_STAMPS = $(BUILD_DIRS:=-stamp)

# Pseudo-IDL files that are used to generate the tree manipulators.

TREE_PIDL_SPECS = src/core/tree/ocarina-me_aadl-aadl_tree-nodes.idl	\
	src/core/tree/ocarina-me_aadl-aadl_instances-nodes.idl		\
	src/core/tree/ocarina-me_aadl_ba-ba_tree-nodes.idl		\
	src/core/tree/ocarina-me_real-real_tree-nodes.idl		\
	src/core/tree/ocarina-me_ao4aadl-ao4aadl_tree-nodes.idl		\
	src/core/tree/ocarina-me_aadl_ema-ema_tree-nodes.idl            \
	src/backends/ocarina-backends-ada_tree-nodes.idl		\
	src/backends/ocarina-backends-c_tree-nodes.idl			\
	src/backends/ocarina-backends-asn1_tree-nodes.idl		\
	src/backends/ocarina-backends-xml_tree-nodes.idl		\
	src/backends/ocarina-backends-pn-nodes.idl			\
	src/backends/ocarina-backends-lnt-nodes.idl			\
	src/backends/ocarina-backends-mast_tree-nodes.idl

TREE_ADA_SPECS = ${addprefix $(top_builddir)/, $(TREE_PIDL_SPECS:.idl=.ads)}
TREE_ADA_BODIES = ${addprefix $(top_builddir)/, $(TREE_PIDL_SPECS:.idl=.adb)}
TREE_PIDL_STAMPS = ${addprefix $(top_builddir)/, $(TREE_PIDL_SPECS:.idl=.idl-stamp)}

# Tree manipulator generation mode:
#  hard : maintainer mode or packaging mode
#  soft : user mode
# This variable has to be set to "hard" by default and overriden by
# calls to make 'GEN_MODE=...'

GEN_MODE = hard

# Various tools

MKNODES_SRC_DIR = $(top_srcdir)/tools/mknodes
MKNODES_BUILD_DIR = $(top_builddir)/tools/mknodes
GENERATE_TREE = $(top_srcdir)/tools/generate_tree.sh

DISTCLEANFILES = \
	$(TREE_ADA_SPECS) \
	$(TREE_ADA_BODIES) \
	$(TREE_PIDL_STAMPS)

PARALLEL_GNATMAKE_FLAGS = -j4

all-local: $(BUILD_DIRS) maybe-generate-tree-files $(PYTHON_TARGET) $(BUILD_STAMPS)

$(BUILD_STAMPS): %-build:
	$(INIT_ADA_PROJECT_PATH) $(GPRBUILD) \
	  $(PARALLEL_GNATMAKE_FLAGS) -P $* $(GNATFLAGS) $(GNAT_LIB_FLAGS) \
	  -XABS_SRC_DIR="$(PL_DEP_ABS_SRC_DIR)" \
	  -XABS_BUILD_DIR="$(PL_DEP_ABS_BUILD_DIR)" \
	  $(AP_FLAGS);

gnatcheck: %-build:
	ADA_PROJECT_PATH=$(top_srcdir)/projects:$(ADA_PROJECT_PATH) \
	gnat check -Pocarina-main.gpr -U $(GNATFLAGS)  \
	  -XABS_SRC_DIR="$(PL_DEP_ABS_SRC_DIR)" \
	  -XABS_BUILD_DIR="$(PL_DEP_ABS_BUILD_DIR)"

gnatmetric: %-build:
	ADA_PROJECT_PATH=$(top_srcdir)/projects:$(ADA_PROJECT_PATH) \
	gnat metric -Pocarina-main.gpr -U $(GNATFLAGS)  \
	  -XABS_SRC_DIR="$(PL_DEP_ABS_SRC_DIR)" \
	  -XABS_BUILD_DIR="$(PL_DEP_ABS_BUILD_DIR)"

clean-local: $(BUILD_DIRS) $(CLEAN_STAMPS) clean-mknodes

# XXX creating/deleting ocarina-configuration.adb is to please
# gnatclean that requires this file to be present to delete files ...

$(CLEAN_STAMPS): %-clean:
	$(INIT_ADA_PROJECT_PATH) $(GNATCLEAN) -P $* $(GNATFLAGS) $(GNAT_LIB_FLAGS)\
	  -XABS_SRC_DIR="$(PL_DEP_ABS_SRC_DIR)" \
	  -XABS_BUILD_DIR="$(PL_DEP_ABS_BUILD_DIR)" \
	  $(AP_FLAGS) ;

distclean-local:
	for d in $(BUILD_DIRS); do \
	  echo "Deleting directory: $${d}"; \
	  rm -rf $${d}; \
	  rm -f $${d}-stamp; \
	done

$(BUILD_DIRS): $(BUILD_DIRS_STAMPS)
	@echo "Creating directory: $@"
	-mkdir -p $@

$(BUILD_DIRS_STAMPS):
	-touch $@

maybe-generate-tree-files: build-mknodes
	$(MAKE) 'GEN_MODE=soft' $(TREE_ADA_SPECS)

# Mknodes needs to be compiled using the native GNATMAKE since it is
# used only on the host.

build-mknodes:
	if [ -f $(MKNODES_SRC_DIR)/mknodes.gpr ]; then \
	   mkdir -p $(MKNODES_BUILD_DIR)/objects; \
	   $(INIT_ADA_PROJECT_PATH) \
	     $(GPRBUILD) -P $(MKNODES_SRC_DIR)/mknodes.gpr \
	     -XABS_SRC_DIR="$(PL_DEP_ABS_SRC_DIR)" \
	     -XABS_BUILD_DIR="$(PL_DEP_ABS_BUILD_DIR)" \
	     $(AP_FLAGS) \
	     $(GNATFLAGS) $(GNAT_LIB_FLAG); \
	fi

clean-mknodes:
	if [ -f $(MKNODES_SRC_DIR)/mknodes.gpr ]; then \
	     $(INIT_ADA_PROJECT_PATH) \
	     $(GNATCLEAN) -P $(MKNODES_SRC_DIR)/mknodes.gpr \
	     -XABS_SRC_DIR="$(PL_DEP_ABS_SRC_DIR)" \
	     -XABS_BUILD_DIR="$(PL_DEP_ABS_BUILD_DIR)" \
	     $(AP_FLAGS); \
	   rm -rf $(MKNODES_BUILD_DIR)/objects; \
	fi

$(top_builddir)/%.ads $(top_builddir)/%.adb: $(top_srcdir)/%.idl $(top_builddir)/%.idl-stamp
	MKNODES_DIR="$(MKNODES_BUILD_DIR)" \
	UHEADERS_DIR="$(top_builddir)/support" \
	OUTPUT_DIR="${dir $@}" \
	GNU_MAKE="${GNU_MAKE}" \
	CYGPATH_W="${CYGPATH_W}" \
	  $(GENERATE_TREE) $(GEN_MODE) $<

%.idl-stamp:
	mkdir -p ${dir $@}
	touch $*.idl-stamp

python_tree:
	echo "*****"
	$(MKNODES_BUILD_DIR)/mknodes -t python $(top_srcdir)/src/core/tree/ocarina-me_aadl-aadl_tree-nodes.idl	\
	 -D $(top_builddir)/src/python
	$(MKNODES_BUILD_DIR)/mknodes -t python $(top_srcdir)/src/core/tree/ocarina-me_aadl-aadl_instances-nodes.idl	\
	 -D $(top_builddir)/src/python
	echo "*****"

dist-hook: build-mknodes $(TREE_ADA_SPECS) $(TREE_ADA_BODIES)

debug-makefile:
	@echo "=========================================================="
	@echo "TREE_PIDL_SPECS = $(TREE_PIDL_SPECS)"
	@echo "=========================================================="
	@echo "TREE_PIDL_STAMPS = $(TREE_PIDL_STAMPS)"
	@echo "=========================================================="
	@echo "TREE_ADA_SPECS = $(TREE_ADA_SPECS)"
	@echo "=========================================================="
	@echo "TREE_ADA_BODIES = $(TREE_ADA_BODIES)"
	@echo "=========================================================="
