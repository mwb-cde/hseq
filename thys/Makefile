
###
# Variables
###

# SUBDIRS: The sub-directories to build (in order)
SUBDIRS =

# BUILDSCRIPT: The name of the build script
BUILDSCRIPT = buildall.ml

# HSEQ: The name of the theorem prover executable
HSEQ=hseq

# HSEQB: The name of the theorem prover scripter
HSEQB=hseqb

# HSEQLIB: The include directory for $(HSEQ) (before installation and 
# relative to the src directory).
HSEQLIB=lib

####
# Don't modify anything below this line
####

##
# Constants
##

# CONFIGDIR: The configuration directory
CONFIGDIR ?= ../config

# CONFIGFILE: Configuration data file 
CONFIGFILE = $(CONFIGDIR)/data.make

# SUBCONFIGDIR: The configuration directory for sub-directories
# (relative to a subdirectory)
SUBCONFIGDIR ?= ../config

# SRCDIR: The source directory
SRCDIR ?= ../src

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
IPREFIX = $(strip $(Prefix))

# IBASEDIR: Installation directory
IBASEDIR = $(BaseDir)

# ITHYDIR: Theories installation directory
ITHYDIR = $(ThyDir)

# IDOCDIR: The documentation installation directory
IDOCDIR ?= $(IBASEDIR)/doc

# HTMLDIR: HTML Documentation directory
HTMLDIR ?= $(IDOCDIR)/html

# INFODIR: Info Documentation directory
INFODIR ?= $(IDOCDIR)/info

##
# Read command definitions
##

include $(CONFIGDIR)/Makefile.os


##
# Build commands
##

MAKEOPTIONS += 

#ifdef TOOLBOX
MAKEOPTIONS += TOOLBOX=$(TOOLBOX)
#endif

# BAREMAKE: The make with not options
BAREMAKE = make 

# DMAKE: The make to build sub-directories with.
DMAKE = make $(MAKEOPTIONS)

# MAKECLEAN: The make to clean up with
MAKECLEAN = make NODEPEND=true $(MAKEOPTIONS)

###
# Compiler definitions
###

##
# Variables
##

# INCLUDE: The include directives
INCLUDE = --include $(LIBDIR)

# OPTIONS: Compiler options
OPTIONS = -I $(LIBDIR) -I $(SRCDIR)

# HSEQC: The hseq compiler
HSEQC = $(SRCDIR)/$(HSEQB) --bin $(SRCDIR)/$(HSEQ) $(INCLUDE)

###
# Required build targets
###

.PHONY: all
.PHONY: thys
.PHONY: install
.PHONY: install-thys
.PHONY: doc  

.PHONY: clean
.PHONY: reallyclean

# all: Build everything
all: thys

#doc: Buld documentation
doc:

#install: Install everything
install: install-thys

###
# Clean up
###

clean: 
	$(RM) *~
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) clean;) $(SKIP)

reallyclean: clean
	$(RM) *.tho *.cmo *.cmi
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; ) $(SKIP)

###
# Specialized targets
###

thys:
	$(HSEQC) $(BUILDSCRIPT) 

.PHONY: install-thys
install-thys: thys
	-$(MKDIR) $(ITHYDIR)
	-$(COPY) *.tho $(ITHYDIR)
	-$(COPY) *Script.ml $(ITHYDIR)
	-$(COPY) *.cmi $(ITHYDIR)
	-$(COPY) *.cmo $(ITHYDIR)
