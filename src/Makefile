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

# TOOLSDIR: The directory containing the tools
TOOLSDIR = $(SRCDIR)/tools

# TOOLS: The list of tools
TOOLS = hseqb

# LIBCODE: The code to link in to build the library (in order)
export LIBCODE= util.cma tpcore.cma logic.cma \
	parser.cma tpmain.cma boollib.cma simplifier.cma \
	numbers.cma userlib.cma

# TOPCODE: The code to link in to build the executable (in order)
export TOPCODE= tpquote.cma hseqtop.cma

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

###
# Common Targets
###

.PHONY: all  # all: Build everything
.PHONY: lib  # lib: Build the library file
.PHONY: opt  # opt: Build the native code library file
.PHONY: install # install: Install the library and header files.
.PHONY: installopt # installopt: Install the native code library 
                   #             and header files.
.PHONY: doc  #doc: Generate the code documentation


.PHONY: clean
.PHONY: libclean
.PHONY: reallyclean
.PHONY: docclean

##
# Local targets
##

.PHONY: quote # Build the quotation expander
.PHONY: code  # Build the library code
.PHONY: toplevel # Build the toplevel
.PHONY: tools    # Build the additional tools
.PHONY: install-tools # Install the tools


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

all: hseq tools

quote: 
	$(DMAKE) -C quote install

code: quote
	$(foreach subdir, $(LIBDIRS), \
		$(DMAKE) -C $(subdir) install;)

doc:
	$(foreach subdir, $(SUBDIRS), \
		$(DMAKE) -C $(subdir) doc;)

##
# Directories
##

$(IBASEDIR): 
	-$(MKDIR) $(IBASEDIR)

$(IBINDIR): $(IBASEDIR)
	-$(MKDIR) $(IBINDIR)

$(ILIBDIR): $(IBASEDIR)
	-$(MKDIR) $(ILIBDIR)

$(IDOCDIR): $(IBASEDIR)
	-$(MKDIR) $(IDOCDIR)

##
# Executables
##

mkhseq: mkhseq.ml
	$(OCAMLC) -o mkhseq mkhseq.ml


lib: code
	$(DMAKE) -f Make.lib install

toplevel: 
	$(foreach subdir, $(TLDIRS), \
		$(DMAKE) -C $(subdir) install;)

$(BINFILE): lib toplevel
	$(OCAMLMKTOP) -o $(BINFILE) $(LIBFILE) $(TOPCODE) 

install: $(IBASEDIR) $(IBINDIR) $(ILIBDIR) $(BINFILE) install-tools
	$(COPY) $(BINFILE) $(IBINDIR)/$(IBINFILE)
	-$(COPY) $(INITFILE) $(ILIBDIR)
	$(COPY) $(LIBFILE) $(ILIBDIR)
	$(foreach f, $(TOPCODE), $(COPY) $(LIBDIR)/$(f) $(ILIBDIR);)
	-$(COPY) $(LIBDIR)/*.mli $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.ml $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.cmi $(ILIBDIR)

install-doc: $(IDOCDIR)
	-$(COPY) $(DOCDIR)/* $(IDOCDIR)

##
# Targets to build the tools
##

tools: $(IBINDIR)
	$(foreach tool, $(TOOLS),\
		$(DMAKE) -C $(TOOLSDIR) install-$(tool);)

install-tools: $(IBINDIR) tools
	$(foreach tool, $(TOOLS),\
		-$(COPY) $(tool) $(IBINDIR);)

tools-clean:
	$(DMAKE) -C $(TOOLSDIR) clean

tools-reallyclean:
	$(foreach tool, $(TOOLS), $(RM) $(tool);)
	$(DMAKE) -C $(TOOLSDIR) reallyclean

###
# Clean up
###

clean:  tools-clean
	$(RM) *.cmo
	$(RM) *.cmx  
	$(RM) *.o
	$(RM) *~
	$(foreach subdir, quote $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) clean; )

libclean: 
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) libclean; )
	$(MAKECLEAN) -C quote libclean
	$(MAKECLEAN) -C $(LIBDIR) libclean

reallyclean: clean docclean tools-reallyclean
	$(RM) hseq
	$(RM) depend
	$(RM) *.cmi
	$(RM) *.cma 
	$(RM) *.cmxa
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; )
	$(MAKECLEAN) -C quote reallyclean
	$(MAKECLEAN) -C $(LIBDIR) reallyclean

docclean: 
	$(BAREMAKE) -C doc reallyclean
