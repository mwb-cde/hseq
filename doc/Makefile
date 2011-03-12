
###
# Variables
###

# SUBDIRS: The sub-directories to build (in order)
SUBDIRS = quickstart


# Relative path to root of tree.
LEVEL=..

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
include $(LEVEL)/Makefile.common

# Variables which are set in $(CONFIGFILE)
Bin ?= 
Prefix ?= 
BinDir ?= 
BaseDir ?= 
IncludeDir ?= 
LibDir ?= 
ThyDir ?= 

# IPREFIX: The installation prefix
export IPREFIX = $(Prefix)

# IBASEDIR: Installation directory
export IBASEDIR = $(BaseDir)

# IDOCDIR: The documentation installation directory
export IDOCDIR ?= $(IBASEDIR)/doc

# HTMLDIR: HTML Documentation directory
export HTMLDIR ?= $(IDOCDIR)/html

# INFODIR: Info Documentation directory
export INFODIR ?= $(IDOCDIR)/info

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

.PHONY: build-html build-info

.PHONY: install-html install-info

.PHONY: clean
.PHONY: libclean
.PHONY: reallyclean
.PHONY: docclean

##
# Build commands
##

# BAREMAKE: The make with no options
export BAREMAKE = $(MAKE)

# DMAKE: The make to build sub-directories with.
export DMAKE = $(MAKE) SUBCONFIGDIR='$(CONFIGDIR)'

# MAKECLEAN: The make to clean up with
export MAKECLEAN = $(MAKE) NODEPEND=true SUBCONFIGDIR='$(CONFIGDIR)'

###
# Compiler definitions
###

##
# Variables
##

# INCLUDE: The include directives
export INCLUDE = 

# OPTIONS: Compiler options
export OPTIONS = $(INCLUDE)

###
# The build targets
###

# all: Build html
all: info html

#
# HTML
# 

build-html: $(DOCSRC)
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) build-html ;) $(SKIP)

html: build-html

#
# GNU Info
# 

build-info: $(DOCSRC)
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) build-info ;) $(SKIP)

info: build-info


###
# Installation targets
###

ifdef IDOCDIR
$(IDOCDIR):
	-$(MKDIR) $(IDOCDIR)
endif
ifdef HTMLDIR
$(HTMLDIR): $(IDOCDIR)
	-$(MKDIR) $(HTMLDIR)
endif
ifdef INFODIR
$(INFODIR): $(IDOCDIR)
	-$(MKDIR) $(INFODIR)
endif

install-html: html $(HTMLDIR)
	-cp -Rf html/* $(HTMLDIR)

install-info: info $(INFODIR)
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



