
###
# Variables
###

# SUBDIRS: The sub-directories to build (in order)
SUBDIRS = quickstart


####
# Don't modify anything below this line
####

##
# Constants
##

# SRCDIR: The source directory
SRCDIR ?= .

# CONFIGDIR: The configuration directory
CONFIGDIR ?= $(SRCDIR)/config

# CONFIGFILE: Configuration data file 
CONFIGFILE = $(CONFIGDIR)/data.make

# SUBCONFIGDIR: The configuration directory for sub-directories
# (relative to a subdirectory)
SUBCONFIGDIR ?= ../config

# LIBDIR: The location of the compiled files
LIBDIR ?= $(SRCDIR)/lib

###
# Configuration variables
# Set by data.make
###

# Read from $(CONFIGFILE)
ifdef CONFIGFILE
include $(CONFIGFILE)
endif

# Variables which are set in $(CONFIGFILE)
Bin ?= 
Prefix ?= 
BinDir ?= 
BaseDir ?= 
IncludeDir ?= 
LibDir ?= 
ThyDir ?= 

# IPREFIX: The installation prefix
IPREFIX = $(Prefix)

# IBASEDIR: Installation directory
IBASEDIR = $(BaseDir)

# IDOCDIR: The documentation installation directory
IDOCDIR ?= $(IBASEDIR)/doc

# HTMLDIR: HTML Documentation directory
HTMLDIR ?= $(IDOCDIR)/html

# INFODIR: Info Documentation directory
INFODIR ?= $(IDOCDIR)/info

##
# Read command definitions
##

include ../config/Makefile.os

##
# Build commands
##

# BAREMAKE: The make with no options
BAREMAKE = $(MAKE)

# DMAKE: The make to build sub-directories with.
DMAKE = $(MAKE) SUBCONFIGDIR='$(CONFIGDIR)'

# MAKECLEAN: The make to clean up with
MAKECLEAN = $(MAKE) NODEPEND=true SUBCONFIGDIR='$(CONFIGDIR)'

###
# Compiler definitions
###

##
# Variables
##

# INCLUDE: The include directives
INCLUDE = 

# OPTIONS: Compiler options
OPTIONS = $(INCLUDE)

###
# The build targets
###


# all: Build html
all: html

.PHONY: html
html: $(DOCSRC)
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) install-html;) $(SKIP)

.PHONY: info
info: $(DOCSRC)
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) install-info;) $(SKIP)

install-html: html
	-$(MKDIR) $(IDOCDIR)
	-$(MKDIR) $(HTMLDIR)
	-cp -Rf html/* $(HTMLDIR)

install-info: info
	-$(MKDIR) $(IDOCDIR)
	-$(MKDIR) $(INFODIR)
	-cp -Rf info/* $(INFODIR)

install: install-html install-info

###
# Clean up
###

.PHONY: clean
clean: 
	-$(RM) *~
	-$(foreach subdir, \
		$(SUBDIRS), $(MAKECLEAN) -C $(subdir) clean; )$(SKIP)

.PHONY: reallyclean
reallyclean: clean
	-$(RM) source/*
	-$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; ) $(SKIP)



