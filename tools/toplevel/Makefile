########################################################## --*- Makefile -*--
# Copyright (c) 2011-2021 Matthew Wahab <mwb.cde@gmail.com>
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
#		unix.cma dynlink.cma
#		hseq.cma hseqtop.cma
#		-o hseq

# hseq_SOURCES: List of source file names, without suffixes.
#hseq_SOURCES=<no sources>

# object_INCLUDE: Directories to include when compiling [object].
hseq_INCLUDE=$(SRC_ROOT)/hseq $(SRC_ROOT)/hsequser \
	$(SRC_ROOT)/tools/hseqtop +unix +dynlink

lm-hseq-include=$(foreach l,$(hseq_INCLUDE),-I $(l) )

# object_LIBS: Libraries to include when linking [object].
hseq_LIBS=hseq.cma hsequser.cma hseqtop.cma

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
		unix.cma dynlink.cma \
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
