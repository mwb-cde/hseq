########################################################## --*- Makefile -*--
# Makefile.module - Makefile to build modules
# Copyright 2011-2016, Matthew Wahab <mwb.cde@gmail.com>
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
#####

#####
# Required definitions
#

# PROJ_ROOT: Relative path to HSeq root.
PROJ_ROOT=../..

#####
# Module building settings
#

# PROGRAMS: List of programs to build.
PROGRAMS=hseq

# Rule for building hseq toplevel is:
#    ocamlmktop -custom $(GLOBAL_OCAMLC_FLAGS)
#		nums.cma unix.cma dynlink.cma
#		hseq.cma hseqtop.cma
#		-o hseq

# hseq_SOURCES: List of source file names, without suffixes.
#hseq_SOURCES=<no sources>

# object_INCLUDE: Directories to include when compiling [object].
hseq_INCLUDE=$(SRC_ROOT)/hseq $(SRC_ROOT)/hsequser \
	$(SRC_ROOT)/tools/hseqtop

lm-hseq-include=$(foreach l,$(hseq_INCLUDE),-I $(l) )

# object_LIBS: Libraries to include when linking [object].
hseq_LIBS=nums.cma unix.cma dynlink.cma \
	hseq.cma hsequser.cma hseqtop.cma

# DISABLE_NATIVECODE: If yes, don't generate native-code
hseq_DISABLE_NATIVECODE=true

###
# Makefile commands

# EXTRA_CLEAN: Command to add to the clean target.
EXTRA_CLEAN=$(RM) hseq

# Use a custom build target
hseq_CUSTOM_BUILD=yes
hseq:
	$(OCAMLMKTOP) -custom  $(GLOBAL_OCAMLC_FLAGS) \
		$(lm-hseq-include) \
		nums.cma unix.cma dynlink.cma \
		-o hseq \
		hseq.cma hsequser.cma hseqtop.cma


######################################################################
# DO NOT CHANGE ANYTHING BELOW THIS LINE
######################################################################

###
# Include common definitions

# Test for PROJ_ROOT being defined.
ifndef PROJ_ROOT
$(error "PROJ_ROOT Must be set to relative path to HSeq root")
endif

include $(PROJ_ROOT)/Makefile.rules
