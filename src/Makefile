###
# Variables
###

# BINFILE: The name of the executable
BINFILE = hseq

# BINFILE: The name of the library
LIBFILE = hseqlib.cma

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
SUBDIRS = util tpcore logic parser \
	tpmain booleans simplifier numbers userlib

# LIBCODE: The code to link in to build the library (in order)
LIBCODE= util.cma tpcore.cma logic.cma \
	parser.cma tpmain.cma boollib.cma simplifier.cma \
	numbers.cma userlib.cma

# CODE: The code to link in to build the executable (in order)
CODE= tpquote.cma

####
# Don't modify anything below this line
####

##
# Constants
##

# SRCDIR: The source directory
SRCDIR ?= .

# BUILDDIR: The top of the build directory (relative to a sub-directory).
BUILDDIR ?= $(SRCDIR)/..

# CONFIGDIR: The configuration directory
CONFIGDIR ?= $(BUILDDIR)/config

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
export IBINFILE = $(BINFILE)

# IPREFIX: The installation prefix
export IPREFIX = $(Prefix)

# IBINDIR: Installation binary directory
export IBINDIR = $(BinDir)

# IBASEDIR: Installation directory
export IBASEDIR = $(strip $(BaseDir))

# ILIBDIR: Installation library directory
export ILIBDIR = $(LibDir)

# ITHYDIR: Installation theory directory
export ITHYDIR = $(ThyDir)

# IDOCDIR: Installation documentation directory
export IDOCDIR = $(IBASEDIR)/doc/source

# FASTCOMP: whether to use the fast compilers
export FASTCOMP = $(FastCompilers)



##
# Build commands
##

# CMDSFILE: File defined OS Commands to use in the build
CMDSFILE ?= $(CONFIGDIR)/Makefile.os

# SUBMAKEFILE: Makefile for subdirectories
SUBMAKEFILE ?= $(CONFIGDIR)/Makefile.sub

# SUBDATA: File containing definitions for subdirectories
SUBDATA ?= Makefile.def

# MAKEOPTIONS: Options to pass to make
MAKEOPTIONS += SUBCONFIGDIR='$(CONFIGDIR)'

#ifdef TOOLBOX
#MAKEOPTIONS += TOOLBOX=$(TOOLBOX)
#endif

# BAREMAKE: The make with not options
BAREMAKE = make 

# DMAKE: The make to build sub-directories with.
DMAKE = make $(MAKEOPTIONS)

# MAKECLEAN: The make to clean up with
MAKECLEAN = make NODEPEND=true $(MAKEOPTIONS)

##
# Read command definitions
##

include $(CONFIGDIR)/Makefile.os

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

# Flags:
# FASTCOMP: Use the fast compilers

# BAREOCAMLC: The bare OCaml bytecode compiler
export BAREOCAMLC=ocamlc

# OCAMLC: The OCaml byte code compiler
export OCAMLC= $(BAREOCAMLC) $(OPTIONS) 

# OCAMLNATC: The OCaml native code compiler
OCAMLNATC= ocamlnat $(OPTIONS) 

# OCAMLDEP: The OCaml dependency generator
OCAMLDEP = ocamldep $(PPOPTIONS)

# OCAMLDOC: The documentation generator
OCAMLDOC = ocamldoc 

ifeq ($(FASTCOMP), true)

# BAREOCAMLC: The bare OCaml bytecode compiler
export BAREOCAMLC= ocamlc.opt 

# OCAMLC: The OCaml bytecode compiler
export OCAMLC= ocamlc.opt $(OPTIONS) 

# OCAMLCNAT: The OCaml native code compiler
OCAMLCNAT= ocamlnat $(OPTIONS) 

# OCAMLDEP: The OCaml dependency generator
OCAMLDEP = ocamldep.opt $(PPOPTIONS)

# OCAMLDOC: The documentation generator
OCAMLDOC = ocamldoc.opt 

endif

# OCAMLlink: The OCaml byte-code linker
export OCAMLlink= $(BAREOCAMLC) -custom -a $(INCLUDE) 

# OCAMLNATlink: The OCaml native-code linker
OCAMLNATlink= $(OCAMLCNAT) -c $(INCLUDE) $(OCLIBS) 

# OCAMLCPP : the preprocesor
OCAMLCPP = camlp4 pa_o.cmo pr_o.cmo pa_extend.cmo pa_macro.cmo -I$(CONFIGDIR) 

# OCAMLMKTOP: Make a toplevel
OCAMLMKTOP = ocamlmktop -custom $(INCLUDE) $(OCINCLUDE) $(OCLIBS)

###
# The build targets
###

all: hseq

.PHONY: quote
quote: 
	$(DMAKE) -C quote install

.PHONY: code
code: quote
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) install;)

.PHONY: doc
doc:
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) doc;)

##
# Executables
##

mkhseq: mkhseq.ml
	$(OCAMLC) -o mkhseq mkhseq.ml

$(LIBFILE): code
	ocamlc.opt -a -custom -o $(LIBFILE) $(INCLUDE) $(LIBCODE)
#	$(OCAMLlink) -o $(LIBFILE) $(LIBCODE)

$(BINFILE): code $(LIBFILE)
	$(OCAMLMKTOP) -o $(BINFILE) $(CODE) $(LIBFILE)

install: $(BINFILE)
	-$(MKDIR) $(IBASEDIR)
	-$(MKDIR) $(IBINDIR)
	$(COPY) $(BINFILE) $(IBINDIR)/$(IBINFILE)
	-$(MKDIR) $(ILIBDIR)
	-$(COPY) $(INITFILE) $(ILIBDIR)
	$(foreach f, $(CODE), $(COPY) $(LIBDIR)/$(f) $(ILIBDIR))
	$(COPY) $(LIBFILE) $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.mli $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.ml $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.cmi $(ILIBDIR)
#	-$(COPY) $(LIBDIR)/*.cmo $(ILIBDIR)
#	-$(COPY) $(LIBDIR)/*.cma $(ILIBDIR)

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
	$(foreach subdir, quote $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) clean; )

.PHONY: libclean
libclean: 
	$(RM) *.cmi
	$(RM) *.cma 
	$(RM) *.cmxa
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) libclean; )
	$(MAKECLEAN) -C quote libclean
	$(MAKECLEAN) -C $(LIBDIR) libclean

.PHONY: reallyclean
reallyclean: docclean
	$(RM) hseq
	$(RM) depend
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; )
	$(MAKECLEAN) -C quote reallyclean
	$(MAKECLEAN) -C $(LIBDIR) reallyclean

.PHONY: docclean
docclean: 
	$(BAREMAKE) -C doc reallyclean
