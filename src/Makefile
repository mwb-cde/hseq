###
# Variables
###

# BINFILE: The name of the executable
export BINFILE = hseq

# LIBFILE: The name of the library
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
TOOLS = hseqb hseqc

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

# SRCCONFIGDIR: The source directory configuration
SRCCONFIGDIR ?= $(SRCDIR)/config

# STDDEFS_FILE: Common definitions
STDDEFS_FILE = $(SRCCONFIGDIR)/Makefile.std

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
export IDOCDIR = $(IBASEDIR)/doc

# ISDOCDIR: Installation source code documentation directory
export ISDOCDIR = $(IDOCDIR)/source

###
# Standard build settings.
###

# CMDSFILE: File defined OS Commands to use in the build
export CMDSFILE ?= $(CONFIGDIR)/Makefile.os

# FAST: whether to use the fast compilers
export FAST = $(FastCompilers)

# Read from $(CONFIGFILE)
ifdef STDDEFS_FILE
include $(STDDEFS_FILE)
endif

##
# Local targets
##

.PHONY: quote # Build the quotation expander
.PHONY: code  # Build the library code
.PHONY: toplevel # Build the toplevel
.PHONY: tools    # Build the additional tools
.PHONY: install-tools # Install the tools

.PHONY: installopt # installopt: Install the native code library 
                   #             and header files.

##
# Build commands
##


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

#include $(CONFIGDIR)/Makefile.os

###
# Compiler definitions
###

##
# Directories and files
##

# HEADERSDIR: The location of the compiled header files
export HEADERSDIR ?= $(SRCDIR)/lib

# LIBDIR: The location of the compiled files
export LIBDIR ?= $(SRCDIR)/lib

# DOCDIR: Source code documentation directory
export DOCDIR ?= $(SRCDIR)/doc

##
# Compiler settings
##

# PPCODE: The preprocessor code
#export PPCODE ?= 

# PPOPTIONS: Preprocessor options 
# To avoid using the preprocessor, 
# set "PPOPTIONS =" (i.e. an empty value)
#ifneq (($strip $(PPOPTIONS)), "")
#export PPOPTIONS ?= 
#endif

# OPTIONS: Compiler options
export OPTIONS = $(PPOPTIONS)

# INCLUDE: The include directives
export INCLUDE = -I $(HEADERSDIR)

# Compiler options
OCAMLC_OPTIONS = $(OPTIONS)
OCAMLNAT_OPTIONS = $(OPTIONS)

# Linker options
OCAMLLINK_OPTIONS = -custom $(INCLUDE)
OCAMLNATLINK_OPTIONS = -custom $(INCLUDE)

# Toplevel linker options
OCAMLMKTOP_OPTIONS = -custom $(INCLUDE) $(OCINCLUDE) $(OCLIBS)

# ocamldep options
OCAMLDEP_OPTIONS = $(OPTIONS)

# ocamldoc options
OCAMLDOC_OPTIONS = $(OPTIONS)


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

make-ibasedir:
	-$(MKDIR) $(IBASEDIR)

make-ibindir: make-ibasedir
	-$(MKDIR) $(IBINDIR)

make-ilibdir: make-ibasedir
	-$(MKDIR) $(ILIBDIR)

make-idocdir: make-ibasedir
	-$(MKDIR) $(IDOCDIR)

make-isdocdir: make-idocdir
	-$(MKDIR) $(ISDOCDIR)

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

install: make-ibasedir make-ibindir make-ilibdir $(BINFILE) install-tools
	$(COPY) $(BINFILE) $(IBINDIR)/$(IBINFILE)
	-$(COPY) $(INITFILE) $(ILIBDIR)
	$(COPY) $(LIBFILE) $(ILIBDIR)
	$(foreach f, $(TOPCODE), $(COPY) $(LIBDIR)/$(f) $(ILIBDIR);)
	-$(COPY) $(LIBDIR)/*.mli $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.ml $(ILIBDIR)
	-$(COPY) $(LIBDIR)/*.cmi $(ILIBDIR)

install-doc: make-isdocdir
	-$(COPY) $(DOCDIR)/* $(ISDOCDIR)

##
# Targets to build the tools
##

tools: make-ibindir
	$(foreach tool, $(TOOLS),\
		$(DMAKE) -C $(TOOLSDIR) install-$(tool);)

install-tools: make-ibindir tools
	$(foreach tool, $(TOOLS),\
		$(COPY) $(tool) $(IBINDIR);)

tools-clean:
	$(DMAKE) -C $(TOOLSDIR) clean

tools-reallyclean:
	$(foreach tool, $(TOOLS), $(RM) $(tool);)
	$(DMAKE) -C $(TOOLSDIR) reallyclean

###
# Clean up
###

clean:  tools-clean
	$(RM) *.cmo *.cmx *.o *~
	$(foreach subdir, quote $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) clean; )

libclean: 
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) libclean; )
	$(MAKECLEAN) -C quote libclean
	$(MAKECLEAN) -C $(LIBDIR) libclean

reallyclean: clean docclean tools-reallyclean
	$(RM) hseq depend
	$(RM) *.cmi *.cma *.cmxa
	$(foreach subdir, $(SUBDIRS), \
		$(MAKECLEAN) -C $(subdir) reallyclean; )
	$(MAKECLEAN) -C quote reallyclean
	$(MAKECLEAN) -C $(LIBDIR) reallyclean

docclean: 
	$(BAREMAKE) -C doc reallyclean
