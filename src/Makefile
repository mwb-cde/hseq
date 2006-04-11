###
# Variables
###

# BINFILE: The name of the executable
BINFILE = hseq

# INITFILE: The name of the initialising file
INITFILE = hseqstart.ml

# BINFILE: The name of the native-code executable
OPTFILE =

# OCINCLUDE: Required OCaml include directories
OCINCLUDE = -I +camlp4

# OCLIBS: Required OCaml libraries.
OCLIBS = nums.cma unix.cma camlp4o.cma pa_extend.cmo q_MLast.cmo

# OCOPTLIBS: The optimised required OCaml libraries.
OCOPTLIBS =

# SUBDIRS: The sub-directories to build (in order)
SUBDIRS = quote util util tpcore logic parser \
	tpmain simplifier numbers userlib

# CODE: The code to link in to build the executable (in order)
CODE= tpquote.cma util.cma tpcore.cma logic.cma \
	parser.cma tpmain.cma simplifier.cma numbers.cma \
	userlib.cma


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

# HEADERSDIR: The location of the compiled header files
HEADERSDIR ?= $(SRCDIR)/lib

# LIBDIR: The location of the compiled files
LIBDIR ?= $(SRCDIR)/lib

# DOCDIR: Source code documentation directory
DOCDIR ?= $(SRCDIR)/doc

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

# IBINFILE: The installed binary
IBINFILE = $(BINFILE)

# IPREFIX: The installation prefix
IPREFIX = $(Prefix)

# IBINDIR: Installation binary directory
IBINDIR = $(BinDir)

# IBASEDIR: Installation directory
IBASEDIR = $(strip $(BaseDir))

# ILIBDIR: Installation library directory
ILIBDIR = $(LibDir)

# ITHYDIR: Installation theory directory
ITHYDIR = $(ThyDir)

# IDOCDIR: Installation documentation directory
IDOCDIR = $(IBASEDIR)/doc/source

##
# Read command definitions
##

include $(CONFIGDIR)/Makefile.os

##
# Build commands
##

MAKEOPTIONS += SUBCONFIGDIR='$(CONFIGDIR)'

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

# PPCODE: The preprocessor code
PPCODE ?= $(LIBDIR)/tpquote.cma

# PPOPTIONS: Preprocessor options 
# To avoid using the preprocessor, 
# set "PPOPTIONS =" (i.e. an empty value)
ifneq (($strip $(PPOPTIONS)), "")
PPOPTIONS ?= -pp "camlp4o q_MLast.cmo $(PPCODE) pa_extend.cmo" -I +camlp4
endif

# OPTIONS: Compiler options
OPTIONS = $(PPOPTIONS)

# INCLUDE: The include directives
INCLUDE = -I $(HEADERSDIR)

##
# Compiler Commands
##

OCAMLC=ocamlc $(OPTIONS) 
OCAMLMKTOP = ocamlmktop -custom $(INCLUDE) $(OCINCLUDE) $(OCLIBS)
OCAMLlink=$(OCAMLC) -c $(INCLUDE) $(OCINCLUDE) $(OCLIBS) 
OCAMLoptlink=ocamlopt -c $(INCLUDE) $(LIB) $(OCINCLUDE) $(OCLIBS) 

###
# The build targets
###

all: hseq

.PHONY: code
code: 
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) install;) $(SKIP)

.PHONY: doc
doc:
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) doc;) $(SKIP)

##
# Executables
##

mkhseq: mkhseq.ml
	$(OCAMLC) -o mkhseq mkhseq.ml

hseq: code
	$(OCAMLMKTOP) -o $(BINFILE) $(CODE) 

install: hseq
	-$(MKDIR) $(IBASEDIR)
	-$(MKDIR) $(IBINDIR)
	$(COPY) $(BINFILE) $(IBINDIR)/$(IBINFILE)
	-$(MKDIR) $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.mli $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.ml $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.cmi $(LIBDIR)/*.cmo $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.cmo $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.cma $(ILIBDIR)
	-$(COPY) $(INITFILE) $(ILIBDIR)

install-doc: 
	-$(MKDIR) $(IDOCDIR)
	-$(COPY) $(DOCDIR)/* $(IDOCDIR)

###
# Clean up
###

.PHONY: clean
clean: 
	$(RM) *.cmo
	$(RM) *.cmx  
	$(RM) *.o
	$(RM) *~
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) clean; ) $(SKIP)

.PHONY: libclean
libclean: 
	$(RM) *.cmi
	$(RM) *.cma 
	$(RM) *.cmxa
	$(foreach subdir, \
		$(SUBDIRS), $(MAKECLEAN) -C $(subdir) libclean; ) $(SKIP)
	$(MAKECLEAN) -C $(LIBDIR) libclean

.PHONY: reallyclean
reallyclean: docclean
	$(RM) hseq
	$(RM) depend
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; ) $(SKIP)
	$(MAKECLEAN) -C $(LIBDIR) reallyclean


.PHONY: docclean
docclean: 
	$(BAREMAKE) -C doc reallyclean
