########################################################## --*- Makefile -*--
# Makefile - Toplevel Makefile for  HSeq
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

###
# Variables
###

# SUBDIRS: The sub-directories to build (in order) 
SUBDIRS = src thys

# DOCSUBDIRS: The sub-directories containing documentation
DOCSUBDIRS = doc

####
# Don't modify anything below this line
####

##
# Constants
##

# LEVEL: Relative path to the root of the source tree. (equivalently:
# relative path to the dirctory containing directory config.)
LEVEL:=.

# Read common definitions
include $(LEVEL)/Makefile.common

# IPREFIX: The installation prefix
export IPREFIX = $(Prefix)

# IBASEDIR: Installation directory
export IBASEDIR = $(BaseDir)

# IDOCDIR: The documentation installation directory
export IDOCDIR ?= $(IBASEDIR)/doc

# ILIBDIR: Library installation directory
export ILIBDIR ?= $(IBASEDIR)/lib

# ITHYDIR: Theory installation directory
export ITHYDIR ?= $(IBASEDIR)/thys

# IBINDIR: Binary installation directory
export IBINDIR ?= $(BinDir)

# FASTCOMP: whether to use the fast compilers
export FASTCOMP = $(FastCompilers)

##
# Targets
##

.PHONY: all  # all: Build everything
.PHONY: lib  # lib: Build the library file
.PHONY: opt  # opt: Build the native code library file
.PHONY: install # install: Install the library and header files.
.PHONY: installopt # installopt: Install the native code library 
                   #             and header files.
.PHONY: doc  #doc: Generate the documentation

.PHONY: clean
.PHONY: libclean
.PHONY: reallyclean
.PHONY: docclean

.PHONY: hseq # Build the theorem prover
.PHONY: thys # Build the theories
.PHONY: srcdoc # Build the source documentation 

.PHONY: install-hseq # Install the theorem prover
.PHONY: install-thys # Install the theories
.PHONY: install-doc  # Install the documentation
.PHONY: install-srcdoc # Install the source code documentation

###
# Required build targets
###

# all: Build everything
all: hseq thys doc

# hseq: Build the system
hseq:
	$(DMAKE) -C src all

# thys: Build the theories
thys:
	$(DMAKE) -C thys all

#doc: Buld documentation
doc:
	$(foreach docsubdir, $(SUBDIRS), $(DMAKE) -C $(subdir) doc;)

srcdoc:
	$(DMAKE) -C src doc

#install: Install everything
install: install-hseq install-thys install-doc

# install-hseq: Install the system
install-hseq: hseq
	$(DMAKE) -C src install

# install-thys: Install the thys
install-thys: thys
	$(DMAKE) -C thys install

# install-doc: Install the documentation
install-doc: doc
	$(foreach subdir, $(DOCSUBDIRS), \
		$(DMAKE) -C $(subdir) install;) $(SKIP)

# install-srcdoc: Install the documentation for the source code
install-srcdoc: srcdoc
	$(DMAKE) -C src install-doc

###
# Clean up
###

clean: 
	-$(RM) *~
	$(foreach subdir, $(DOCSUBDIRS), \
		$(MAKE) -C $(subdir) clean;) $(SKIP)
	-$(RM) hseq hseqb hseqc
	-$(RM) lib/*

distclean: clean
	$(MAKE) -C thys reallyclean
	$(foreach subdir, $(DOCSUBDIRS), \
		$(MAKE) -C $(subdir) reallyclean; ) $(SKIP)
	-$(RM) hseq hseqb hseqc
	-$(RM) lib/*
	-$(RM) config/configure.data
	-$(RM) config/data.make




