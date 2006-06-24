#####
# Makefile for building tools
#####


####
# Variables 
###

# TOOLS: The list of tools to build

TOOLS := hseqb hseqc

##
# Variables for each tool
##

## hseqb

# hseqb_SOURCES : The files, in order, to build the tool from
hseqb_SOURCES := hseqb.ml

# hseqb_OPTIONS: Options to pass to the compiler when building the tool
hseqb_OPTIONS := 

# hseqb_INCLUDE: include directories when building the tool
hseqb_INCLUDE :=

# hseqb_LIBS: libraries to link in for the tool
hseqb_LIBS := unix.cma

# hseqb_TEMP: files generated when building the tool, which can removed
#   after the tool is build
hseqb_TEMP := hseqb.ml

## hseqc

# hseqc_SOURCES : The files, in order, to build the tool from
hseqc_SOURCES := hseqc.ml

# hseqc_OPTIONS: Options to pass to the compiler when building the tool
hseqc_OPTIONS := 

# hseqc_INCLUDE: include directories when building the tool
hseqc_INCLUDE :=

# hseqc_LIBS: libraries to link in for the tool
hseqc_LIBS := 

# hseqc_TEMP: files generated when building the tool, which can removed
#   after the tool is build
hseqc_TEMP := hseqc.ml

##
# Directories
##

# SRCDIR: The source directory (relative to a sub-directory).
SRCDIR = ..

# BUILDDIR: The top of the build directory (relative to a sub-directory).
BUILDDIR ?= $(SRCDIR)/..

# TOPCONFIGDIR: The toplevel configuration directory
TOPCONFIGDIR = $(BUILDDIR)/config

# CONFIGFILE: The configuration file
CONFIGFILE = $(TOPCONFIGDIR)/data.make

# HEADERSDIR: The location of the compiled header files
HEADERSDIR = $(SRCDIR)/lib

# LIBDIR: The location of the compiled files
LIBDIR = $(SRCDIR)/lib

# LIBDIR: The location of the compiled files
BINDIR = $(SRCDIR)

# DOCDIR: Source code documentation directory
DOCDIR = $(SRCDIR)/doc

###
# Configuration variables
# Set by data.make
###

# Read from $(CONFIGFILE)
ifdef CONFIGFILE
-include $(CONFIGFILE)
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
IBINFILE ?= $(BINFILE)

# IPREFIX: The installation prefix
IPREFIX ?= $(Prefix)

# IBINDIR: Installation binary directory
IBINDIR ?= $(BinDir)

# IBASEDIR: Installation directory
IBASEDIR ?= $(strip $(BaseDir))

# ILIBDIR: Installation library directory
ILIBDIR ?= $(LibDir)

# ITHYDIR: Installation theory directory
ITHYDIR ?= $(ThyDir)

# IDOCDIR: Installation documentation directory
IDOCDIR ?= $(IBASEDIR)/doc/source

# FASTCOMP: whether to use the fast compilers
ifndef FASTCOMP
export FASTCOMP = $(FastCompilers)
endif

##
# Load the OS commands
##
include $(TOPCONFIGDIR)/Makefile.os

###
# Compiler definitions
###

##
# Global Variables
##

# OPTIONS: options to pass to the compilers
OPTIONS ?= 

# INCLUDE: include directorives for the compilers
INCLUDE ?= 

# LIBS: Libraries to pass to the compilers
LIBS ?= 

##
# The compilers
##

# Flags:
# FASTCOMP: Use the fast compilers

# BAREOCAML: The bare compiler
export BAREOCAMLC= ocamlc

# OCAMLC: The OCaml byte code compiler
export OCAMLC= ocamlc $(OPTIONS) 

# OCAMLNATC: The OCaml native code compiler
export OCAMLNATC= ocamlopt $(OPTIONS) 

# OCAMLDEP: The OCaml dependency generator
export OCAMLDEP = ocamldep 

# OCAMLDOC: The documentation generator
export OCAMLDOC = ocamldoc 

ifeq ($(FASTCOMP), true)

# OCAMLC: The OCaml bytecode compiler
export OCAMLC= ocamlc.opt $(OPTIONS) 

# OCAMLNATC: The OCaml native code compiler
export OCAMLNATC= ocamlopt.opt $(OPTIONS) 

# OCAMLDEP: The OCaml dependency generator
export OCAMLDEP = ocamldep.opt $(OPTIONS)

# OCAMLDOC: The documentation generator
export OCAMLDOC = ocamldoc.opt 

endif

# OCAMLlink: The OCaml byte-code linker
export OCAMLlink= $(BAREOCAMLC) -a $(INCLUDE) $(LIBS)

# OCAMLNATlink: The OCaml native-code linker
export OCAMLNATlink= $(OCAMLCNAT) -a $(INCLUDE) $(LIBS) 

# OCAMLCPP : the preprocesor
export OCAMLCPP = camlp4 pa_o.cmo pr_o.cmo pa_extend.cmo pa_macro.cmo -I$(TOPCONFIGDIR) 

# OCAMLMKTOP: Make a toplevel
export OCAMLMKTOP = ocamlmktop -custom $(INCLUDE) $(INCLUDE) $(LIBS)

###
# Suffixes and how to handle them
###

.SUFFIXES: .mli .cmi
.SUFFIXES: .ml .cmo
.SUFFIXES: .cmx 
.SUFFIXES: .mlp

.mlp.ml:
	$(OCAMLCPP) -impl $< -o $@

.mli.cmi: 
	$(OCAMLC) -c $(INCLUDE) $<

.ml.cmo: 
	$(OCAMLC) -c $(INCLUDE) $<

.ml.cmx: 
	$(OCAMLOPT) -c $(INCLUDE) $<


###
# Build targets
###

##
# Declare Targets
##

.PHONY: all  # all: Build everything
.PHONY: tools  # lib: Build all tools
.PHONY: install # install: Install everything
.PHONY: doc  #doc: Generate documentation

.PHONY: clean
.PHONY: reallyclean
.PHONY: docclean

##
# Main targets
##

# all: Build everything
all: tools

# tools: Build the tools
tools: $(TOOLS)

# install: Install the library and header files.
install: tools
	$(foreach tool, $(TOOLS), $(COPY) $(tool) $(BINDIR);)

##
# Target to build the tools
##

define build_tool
$(1): $$($(1)_SOURCES)
	$$(OCAMLC) $$($(1)_OPTIONS) -o $(1) $($(1)_INCLUDE) \
		$$($(1)_LIBS) $$($(1)_SOURCES)

.PHONY: install-$(1)
install-$(1): $(1)
	$$(COPY) $(1) $$(BINDIR)

.PHONY: $(1)-clean
$(1)-clean: 
	$$(foreach file, $$($(1)_TEMP), -$$(RM) $$(file);)

endef

$(foreach tool, $(TOOLS), $(eval $(call build_tool,$(tool))))



##
# Cleaning files
##

clean:
	$(RM) *.cmo
	$(RM) *.cmx  
	$(RM) *.o
	$(RM) *~

toolclean: $(foreach tool, $(TOOLS),$(tool)-clean )

reallyclean: clean toolclean
	$(RM) *.cmi
	$(RM) *.cma 
	$(RM) *.cmxa
	$(foreach tool, $(TOOLS), $(RM) $(tool);)

