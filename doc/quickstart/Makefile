########################################################## --*- Makefile -*--
# Copyright (c) 2011-2021 Matthew Wahab <mwb.cde@gmail.com>
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
