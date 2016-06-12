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

# LIBRARIES: List of libraries to build.
LIBRARIES=hseqtop

#####
# Object building settings
#

# object_SOURCES: List of source file names, without suffixes.
hseqtop_SOURCES=unsafe init cli

# object_HEADERS: List of headers, without suffixes. Default is object_SOURCES
object_HEADERS=

# object_INCLUDE: Directories to include when compiling [object].
hseqtop_INCLUDE=+compiler-libs -I $(SRC_ROOT)/quote \
	-I $(SRC_ROOT)/hseq -I $(SRC_ROOT)/hsequser

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
