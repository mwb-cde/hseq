########################################################## --*- Makefile -*--
# Makefile - Makefile to HSeq library
# Copyright 2017, Matthew Wahab <mwb.cde@gmail.com>
#
# Released under the Lesser GPLv3 license:
# ========================================
# This file is part of HSeq.
#
# HSeq is free software; you can redistribute it and/or modify it under the
# terms of the Lesser GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# HSeq is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public License for
# more details.
#
# You should have received a copy of the Lesser GNU General Public License
# along with HSeq.  If not see <http://www.gnu.org/licenses/>.
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

