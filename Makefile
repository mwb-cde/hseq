
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

# CURRDIR: The current directory
CURRDIR ?= .

# CONFIGDIR: The configuration directory
CONFIGDIR ?= ${CURRDIR}/config

# CONFIGFILE: Configuration data file 
CONFIGFILE = ${CONFIGDIR}/data.make

###
# Configuration variables
# Set by data.make
###

# Read from ${CONFIGFILE}
ifdef CONFIGFILE
include $(CONFIGFILE)
endif

# Variables which are set in ${CONFIGFILE}
Bin ?= 
Prefix ?= 
BinDir ?= 
BaseDir ?= 
IncludeDir ?= 
LibDir ?= 
ThyDir ?= 

# IPREFIX: The installation prefix
IPREFIX = $(strip $(Prefix))

# IBASEDIR: Installation directory
IBASEDIR = $(BaseDir)

# IDOCDIR: The documentation installation directory
IDOCDIR ?= $(IBASEDIR)/doc

# ILIBDIR: Library installation directory
ILIBDIR ?= $(IBASEDIR)/lib

# ITHYDIR: Theory installation directory
ITHYDIR ?= $(IBASEDIR)/thys

# IBINDIR: Binary installation directory
IBINDIR ?= $(BinDir)

##
# Build commands
##

# BAREMAKE: The make with no options
BAREMAKE = $(MAKE)

# DMAKE: The make to build sub-directories with.
DMAKE = $(MAKE) 

# MAKECLEAN: The make to clean up with
MAKECLEAN = $(MAKE) NODEPEND=true

###
# Compiler definitions
###

##
# Variables
##

###
# Required build targets
###

# all: Build everything
all: hseq thys doc

# hseq: Build the system
.PHONY: hseq
hseq:
	$(DMAKE) -C src all

# thys: Build the theories
.PHONY: thys
thys:
	$(DMAKE) -C thys all

#doc: Buld documentation
.PHONY: doc
doc:
	$(foreach docsubdir, $(SUBDIRS), $(DMAKE) -C $(subdir) doc;)

.PHONE: srcdoc
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
	$(foreach subdir, $(DOCSUBDIRS), $(DMAKE) -C $(subdir) install;)

# install-srcdoc: Install the documentation for the source code
install-srcdoc: srcdoc
	$(DMAKE) -C src install-doc

###
# Clean up
###

.PHONY: clean
clean: 
	rm -f *~
	$(MAKECLEAN) -C src reallyclean
	$(MAKECLEAN) -C thys clean
	$(foreach subdir, $(DOCSUBDIRS), $(MAKECLEAN) -C $(subdir) clean; )

.PHONY: reallyclean
reallyclean: clean
	rm -f hseq
	rm -rf lib/*
	$(foreach subdir, $(SUBDIRS), $(MAKECLEAN) -C $(subdir) reallyclean; )
	$(foreach subdir, $(DOCSUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; )




