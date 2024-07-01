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
#####

#####
# Required definitions
#

# PROJ_ROOT: Relative path to HSeq root.
PROJ_ROOT=..

#####
# Module building settings
#

# PACKING: List of packing modules to build
LIBRARIES=hsequser

#####
# Object building settings
#

# object_SOURCES: List of source file names, without suffixes.
hsequser_SOURCES=userstate userlib
hsequser_PACK_NAME=HSeqUser
hsequser_INCLUDE=$(SRC_ROOT)/hseq +unix

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
