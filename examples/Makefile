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

# Use a custom dependency rule.
NODEPEND=true

#####
# Module building settings
#

PROGRAMS=libtest sum

#####
# Object building settings
#

# Program libtest

libtest_INCLUDE=$(PROJ_ROOT)/hseq $(PROJ_ROOT)/hsequser
libtest_LIBS=unix.cma hseq.cma hsequser.cma
libtest_NATLIBS=unix.cmxa hseq.cmxa hsequser.cmxa
libtest_SOURCES=libtest

# Program sum

sum_SOURCES=SumExample
sum_CUSTOM_BUILD=yes
sum_FLAGS=-I $(PROJ_ROOT)/hseq -I $(PROJ_ROOT)/hsequser

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

## Custom build rules.
sum: SumExample.ml
	$(PROJ_ROOT)/tools/scripts/hseqc $(sum_FLAGS) $< -o $@

sum.opt: SumExample.ml
	$(PROJ_ROOT)/tools/scripts/hseqc $(sum_FLAGS) --native $< -o $@

.INTERMEDIATE: libtest.mli
libtest.mli: libtest.ml
	echo "" > libtest.mli

