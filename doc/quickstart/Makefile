# --*- Makefile -*--##################################################
# Makefile.module - Makefile to build modules
# Copyright 2011-2016, Matthew Wahab <mwb.cde@gmail.com>
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

###
# The project root
PROJ_ROOT=../..

##
# The document to build
DOCUMENTS=hseq-quickstart

##
# Module settings

# The module include directories
MODULE_INCLUDE=$(PROJ_ROOT)/doc

##
# Settings for each document

# The texinfo sources
hseq-quickstart_TEXINFO=hseq-quickstart.texi

# Document include directory [default: $(MODULE_INCLUDE)]
#hseq-quickstart_INCLUDE=

# Document output name [default: hseq-quickstart]
#hseq-quickstart_OUTPUT=

# Name of the output html directory
#    [default: $(hseq-quickstart).html]
#hseq-quickstart_HTMLDIR=

# Name of the output info directory [default: $(hseq-quickstart).info]
#hseq-quickstart_INFODIR=


######################################################################
# Don't edit anything below this line.
#####

# Test for PROJ_ROOT being defined.
ifndef PROJ_ROOT
$(error "PROJ_ROOT Must be set to relative path to HSeq root")
endif
# Include the standard template.
include $(PROJ_ROOT)/Makefile.rules
