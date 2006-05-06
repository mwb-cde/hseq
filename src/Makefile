###
# Variables
###

# BINFILE: The name of the executable
export BINFILE = hseq

# LIBFILE: The name of the library
#export LIBFILE = hseqlib.cma
export LIBFILE = hseq.cma

# INITFILE: The name of the initialising file
export INITFILE = hseqstart.ml

# BINFILE: The name of the native-code executable
export OPTFILE =

# OCINCLUDE: Required OCaml include directories
export OCINCLUDE = -I +camlp4

# OCLIBS: Required OCaml libraries.
export OCLIBS = nums.cma unix.cma camlp4o.cma pa_extend.cmo q_MLast.cmo

# OCOPTLIBS: The optimised required OCaml libraries.
export OCOPTLIBS =

# SUBDIRS: The sub-directories to build (in order)
export SUBDIRS = util tpcore logic parser \
	tpmain booleans simplifier \
	numbers userlib \
	hseqtop

# LIBDIRS: The sub-directories to build the library from
export LIBDIRS = util tpcore logic parser \
		tpmain booleans simplifier \
		numbers userlib 

# TLDIRS: The sub-directories to build the top-level
TLDIRS = hseqtop

# LIBCODE: The code to link in to build the library (in order)
export LIBCODE= util.cma tpcore.cma logic.cma \
	parser.cma tpmain.cma boollib.cma simplifier.cma \
	numbers.cma userlib.cma

# TOPCODE: The code to link in to build the executable (in order)
export TOPCODE= tpquote.cma hseqtop.cma

# TOPLIBCODE: Inteface to the OCaml toplevel, 
#   to be linked in before anything else
#   (needed to resolve conflicts with identifiers defined
#   in the OCaml toplevel).
#export TOPLIBCODE = toplib.cma

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
#export SUBCONFIGDIR ?= ../config

# HEADERSDIR: The location of the compiled header files
export HEADERSDIR ?= $(SRCDIR)/lib

# LIBDIR: The location of the compiled files
export LIBDIR ?= $(SRCDIR)/lib

# DOCDIR: Source code documentation directory
export DOCDIR ?= $(SRCDIR)/doc

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
export CMDSFILE ?= $(CONFIGDIR)/Makefile.os

# SUBMAKEFILE: Makefile for subdirectories
#export SUBMAKEFILE ?= $(CONFIGDIR)/Makefile.sub

# SUBDATA: File containing definitions for subdirectories
export SUBDATA ?= Makefile.def

# MAKEOPTIONS: Options to pass to make
#MAKEOPTIONS += SUBCONFIGDIR='$(CONFIGDIR)'
#export MAKEOPTIONS

#ifdef TOOLBOX
#MAKEOPTIONS += TOOLBOX=$(TOOLBOX)
#endif

# BAREMAKE: The make with not options
export BAREMAKE = make 

# DMAKE: The make to build sub-directories with.
export DMAKE = make $(MAKEOPTIONS)

# MAKECLEAN: The make to clean up with
export MAKECLEAN = make NODEPEND=true $(MAKEOPTIONS)

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
#export PPCODE ?= $(LIBDIR)/tpquote.cma

# PPOPTIONS: Preprocessor options 
# To avoid using the preprocessor, 
# set "PPOPTIONS =" (i.e. an empty value)
#ifneq (($strip $(PPOPTIONS)), "")
#PPOPTIONS ?= -pp "camlp4o q_MLast.cmo $(PPCODE) pa_extend.cmo" -I +camlp4
#export PPOPTIONS
#endif

# OPTIONS: Compiler options
export OPTIONS = $(PPOPTIONS)

# INCLUDE: The include directives
export INCLUDE = -I $(HEADERSDIR)

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
export OCAMLNATC= ocamlnat $(OPTIONS) 

# OCAMLDEP: The OCaml dependency generator
export OCAMLDEP = ocamldep $(PPOPTIONS)

# OCAMLDOC: The documentation generator
export OCAMLDOC = ocamldoc 

ifeq ($(FASTCOMP), true)

# BAREOCAMLC: The bare OCaml bytecode compiler
export export BAREOCAMLC= ocamlc.opt 

# OCAMLC: The OCaml bytecode compiler
export export OCAMLC= ocamlc.opt $(OPTIONS) 

# OCAMLCNAT: The OCaml native code compiler
export OCAMLCNAT= ocamlnat $(OPTIONS) 

# OCAMLDEP: The OCaml dependency generator
export OCAMLDEP = ocamldep.opt $(PPOPTIONS)

# OCAMLDOC: The documentation generator
export OCAMLDOC = ocamldoc.opt 

endif

# OCAMLlink: The OCaml byte-code linker
export OCAMLlink= $(BAREOCAMLC) -custom -a $(INCLUDE) 

# OCAMLNATlink: The OCaml native-code linker
export OCAMLNATlink= $(OCAMLCNAT) -c $(INCLUDE) $(OCLIBS) 

# OCAMLCPP : the preprocesor
OCAMLCPP = camlp4 pa_o.cmo pr_o.cmo pa_extend.cmo pa_macro.cmo -I$(CONFIGDIR) 
export OCAMLCPP

# OCAMLMKTOP: Make a toplevel
export OCAMLMKTOP = ocamlmktop -custom $(INCLUDE) $(OCINCLUDE) $(OCLIBS)

###
# The build targets
###

all: hseq

.PHONY: quote
quote: 
	$(DMAKE) -C quote install

.PHONY: code
code: quote
	$(foreach subdir, $(LIBDIRS), \
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


.PHONY: lib
lib: code
	$(DMAKE) -f Make.lib install

.PHONY: toplevel
toplevel: 
	$(foreach subdir, $(TLDIRS), \
		$(DMAKE) -C $(subdir) install;)

$(BINFILE): lib toplevel
	$(OCAMLMKTOP) -o $(BINFILE) $(TOPLIBCODE) $(LIBFILE) $(TOPCODE) 

install: $(BINFILE)
	-$(MKDIR) $(IBASEDIR)
	-$(MKDIR) $(IBINDIR)
	$(COPY) $(BINFILE) $(IBINDIR)/$(IBINFILE)
	-$(MKDIR) $(ILIBDIR)
	-$(COPY) $(INITFILE) $(ILIBDIR)
	$(COPY) $(LIBFILE) $(ILIBDIR)
	$(foreach f, $(TOPCODE), $(COPY) $(LIBDIR)/$(f) $(ILIBDIR);)
	-$(COPY) $(LIBDIR)/$(TOPLIBCODE) $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.mli $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.ml $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.cmi $(ILIBDIR)

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
reallyclean: clean docclean
	$(RM) hseq
	$(RM) depend
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; )
	$(MAKECLEAN) -C quote reallyclean
	$(MAKECLEAN) -C $(LIBDIR) reallyclean

.PHONY: docclean
docclean: 
	$(BAREMAKE) -C doc reallyclean
