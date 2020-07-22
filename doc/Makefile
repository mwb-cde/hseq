########################################################## --*- Makefile -*--
# Copyright (c) 2011-2021 Matthew Wahab <mwb.cde@gmail.com>
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
######################################################################

#####
# Definitions and rules for building the documentation
#####

#####
# Required definitions
#

# PROJ_ROOT: Relative path to HSeq root.
PROJ_ROOT=..

###
# Variables
###

# SUBDIRS: The sub-directories to build (in order)
SUBDIRS=quickstart #hseq

####
# Don't modify anything below this line
####


###
# Include common definitions

# Test for PROJ_ROOT being defined.
ifndef PROJ_ROOT
$(error "PROJ_ROOT Must be set to relative path to HSeq root")
endif
include $(PROJ_ROOT)/Makefile.rules
####
# Include the documentation rules
#include Makefile.docrules
