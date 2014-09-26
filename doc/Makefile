# --*- Makefile -*--##################################################
# Makefile.module - Makefile to build modules
# Copyright 2011-2014, Matthew Wahab <mwb.cde@gmail.com>
#
# Released under the Lesser GPLv3 license:
# ========================================
# This file is part of HSeq.
#
# HSeq is free software; you can redistribute it and/or modify it
# under the terms of the Lesser GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# HSeq is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
# License for more details.
#
# You should have received a copy of the Lesser GNU General Public
# License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
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

# DOC_SUBDIRS: The sub-directories to build (in order)
SUBDIRS=quickstart #hseq

####
# Don't modify anything below this line
####


# HTMLDIR: HTML Documentation directory
export HTMLDIR?=$(OBJDOCDIR)/html

# INFODIR: Info Documentation directory
export INFODIR?=$(OBJDOCDIR)/info

###
# Include common definitions

# Test for PROJ_ROOT being defined.
ifndef PROJ_ROOT
$(error "PROJ_ROOT Must be set to relative path to HSeq root")
endif
#include $(PROJ_ROOT)/Makefile.rules
####
# Include the documentation rules
include Makefile.docrules
