########################################################## --*- Makefile -*--
# Makefile - Makefile for tests directory
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

# Test programs to build and run.
PROGRAMS=types terms

#####
# Module building settings
#

LOCAL_OCAMLC_FLAGS:=\
	-I $(PROJ_ROOT)/hseq -I $(PROJ_ROOT)/hsequser \
	nums.cma unix.cma hseq.cma hsequser.cma
LOCAL_OCAMLNAT_FLAGS:=-I $(PROJ_ROOT)/hseq -I $(PROJ_ROOT)/hsequser \
	nums.cmxa unix.cmxa hseq.cmxa hsequser.cmxa

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

define run-test
for prog in $^; do ./$$prog ; done
endef

.PHONEY: test tests
test: $(PROGRAMS)
	$(run-test)

tests: test

.PHONEY: test.opt tests.opt
test.opt: $(PROGRAMS)
	$(run-test)

tests.opt: test.opt


####
# Set the build tools to pre-process the source using CPP.

# CPP: The C preprocessor with warnings disabled.
CPP:='cpp -w'

OCAMLDEP:=$(OCAMLDEP) -pp $(CPP)
OCAMLC:=$(OCAMLC) -pp $(CPP)
OCAMLNAT:=$(OCAMLNAT) -pp $(CPP)

####
# Local build rules.

# Build missing .mli files from the .ml source.
%.mli: %.ml
	$(OCAMLC) -i $(LOCAL_OCAMLC_FLAGS) $< > $@

