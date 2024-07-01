########################################################## --*- Makefile -*--
# Copyright (c) 2017-2021 Matthew Wahab <mwb.cde@gmail.com>
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
######################################################################

#####
# Definitions and rules for building modules.
#
# REQUIREMENTS
# ------------
#
# Must set PROJ_ROOT.
# PROJ_ROOT: Variable containing relative path to top of the source tree.
#
# Must end with
#    include $(PROJ_ROOT)/Makefile.common
#
#
# Variables:
# ----------
#
# Module settings:
#
# SUBDIRS: List of sub-directories to recurse into.
# LIBRARIES: List of libraries to build.
# PACKAGES: List of packages to build.
# PROGRAMS: List of programs to build.
#

#####
# Required definitions
#

# PROJ_ROOT: Relative path to HSeq root.
PROJ_ROOT=..

# Test programs to build and run.
#PROGRAMS=types terms

#####
# Module building settings
#

LOCAL_OCAMLC_FLAGS:=\
	-I $(PROJ_ROOT)/hseq -I $(PROJ_ROOT)/hsequser \
	-I +unix unix.cma hseq.cma hsequser.cma
LOCAL_OCAMLNAT_FLAGS:=-I $(PROJ_ROOT)/hseq -I $(PROJ_ROOT)/hsequser \
	-I +unix unix.cmxa hseq.cmxa hsequser.cmxa

#####
# Object building settings
#

# Build programs from source files of the same name
define program-sources-template

ifeq ($$(origin $(1)_SOURCES),undefined)
$(1)_SOURCES=$(1)
endif

endef # program-sources-template

$(foreach prog,$(PROGRAMS),\
$(eval $(call program-sources-template,$(prog))))

######################################################################
# DO NOT CHANGE ANYTHING IN THIS SECTION
######################################################################

###
# Include common definitions

# Test for PROJ_ROOT being defined.
ifndef PROJ_ROOT
$(error "PROJ_ROOT Must be set to relative path to HSeq root")
endif

include $(PROJ_ROOT)/Makefile.rules

######################################################################
# END OF SECTION
######################################################################

####
# Toplevel rule: Build and run the test programs.
# Assumes a BASH shell.

.PHONY: test tests
test:
	$(PROJ_ROOT)/tests/testdriver.py --config config.txt

tests: test

EXTRA_CLEAN=$(RM) *.o *.exe *.cmi *.cmo *.cmxo *.cma *.cmxa *.log

