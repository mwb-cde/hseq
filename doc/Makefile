########################################################## --*- Makefile -*--
# Makefile.module - Makefile to build modules
# Copyright 11 March, 2011, Matthew Wahab <mwb.cde@gmail.com>
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
# Definitions and rules for building theories.
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
DOC_SUBDIRS = quickstart

####
# Don't modify anything below this line
####

include $(PROJ_ROOT)/Makefile.rules

# IDOCDIR: The documentation installation directory
export OBJDOCDIR?=$(OBJ_ROOT)/doc

# HTMLDIR: HTML Documentation directory
export HTMLDIR?=$(OBJDOCDIR)/html

# INFODIR: Info Documentation directory
export INFODIR?=$(OBJDOCDIR)/info

##
# Read command definitions
##

##
# Targets
##

.PHONY: all  # all: Build everything
.PHONY: install 
.PHONY: doc  #doc: Generate the documentation

.PHONY: html
.PHONY: info

.PHONY: build
.PHONY: build-html build-info

.PHONY: install-html install-info

.PHONY: clean
.PHONY: libclean
.PHONY: reallyclean
.PHONY: docclean

###
# The build targets
###

CUSTOM_TARGET_build=
CUSTOM_TARGET_install=

# all: Build html
all: build

build: build-info build-html

#
# HTML
# 

build-html: $(DOCSRC) $(HTMLDIR)
	$(foreach subdir, $(DOC_SUBDIRS), \
	$(MAKE) -C $(subdir) \
		DOC_DIR=$(SRC_ROOT)/doc HTMLDIR=$(HTMLDIR) build-html ;) $(SKIP)

html: build-html

#
# GNU Info
# 

build-info: $(DOCSRC) $(INFODIR)
	$(foreach subdir, $(DOC_SUBDIRS), \
	$(MAKE) -C $(subdir) \
		DOC_DIR=$(SRC_ROOT)/doc INFODIR=$(INFODIR) build-info ;) $(SKIP)

info: build-info

#
# Directories
#

$(INFODIR):
	$(MKDIR) $(INFODIR)

$(HTMLDIR):
	$(MKDIR) $(HTMLDIR)

###
# Installation targets
###

install-html: html $(HTMLDIR)
	-cp -Rf html/* $(HTMLDIR)

install-info: info $(INFODIR)
	-cp -Rf info/* $(INFODIR)

install: install-html install-info

###
# Clean up
###

EXTRA_CLEAN=$(RM) *~; \
$(foreach subdir, $(DOC_SUBDIRS), $(MAKECLEAN) -C $(subdir) clean; ) \
$(RM) info/*; $(RM) -r html/*





