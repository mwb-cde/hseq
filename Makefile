
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
CONFIGDIR ?= $(CURRDIR)/config

# CONFIGFILE: Configuration data file 
CONFIGFILE = $(CONFIGDIR)/data.make

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
# Read command definitions
##

include $(CONFIGDIR)/Makefile.os

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

##
# Build commands
##

# MAKEOPTIONS += SUBCONFIGDIR='$(CONFIGDIR)'

#ifdef TOOLBOX
export MAKEOPTIONS += TOOLBOX=$(TOOLBOX)
#endif

# BAREMAKE: The make with no options
export BAREMAKE = $(MAKE)

# DMAKE: The make to build sub-directories with.
export DMAKE = make $(MAKEOPTIONS)

# MAKECLEAN: The make to clean up with
export MAKECLEAN = make NODEPEND=true $(MAKEOPTIONS)

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
	$(MAKECLEAN) -C src reallyclean
	$(MAKECLEAN) -C thys clean
	$(foreach subdir, $(DOCSUBDIRS), \
		$(MAKECLEAN) -C $(subdir) clean;) $(SKIP)

reallyclean: clean
	-$(RM) hseq hseqb hseqc
	-$(RM) lib/*
	$(MAKECLEAN) -C thys reallyclean
	$(foreach subdir, $(DOCSUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; ) $(SKIP)




