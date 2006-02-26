
###
# Variables
###

# BINFILE: The name of the executable
BINFILE = hseq

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
CODE= tpquote.cmo util.cma tpcore.cma logic.cma \
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
CONFIGDIR ?= ${SRCDIR}/config

# CONFIGFILE: Configuration data file 
CONFIGFILE = ${CONFIGDIR}/data.make

# SUBCONFIGDIR: The configuration directory for sub-directories
# (relative to a subdirectory)
SUBCONFIGDIR ?= ../config

# HEADERSDIR: The location of the compiled header files
HEADERSDIR ?= ${SRCDIR}/lib

# LIBDIR: The location of the compiled files
LIBDIR ?= ${SRCDIR}/lib

# DOCDIR: Source code documentation directory
DOCDIR ?= ${SRCDIR}/doc

# INSTALLDIR: The directory to install to
INSTALLDIR ?= ./bin

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

# IBINFILE: The installed binary
IBINFILE = $(strip ${BINFILE})

# IPREFIX: The installation prefix
IPREFIX = $(strip ${Prefix})

# IBINDIR: Installation binary directory
IBINDIR = $(strip ${BinDir})

# IBASEDIR: Installation directory
IBASEDIR = $(strip ${BaseDir})

# ILIBDIR: Installation library directory
ILIBDIR = $(strip ${LibDir})

# ITHYDIR: Installation theory directory
ITHYDIR = $(strip ${ThyDir})

##
# Build commands
##

# BAREMAKE: The make with not options
BAREMAKE = make 

# DMAKE: The make to build sub-directories with.
DMAKE = make SUBCONFIGDIR='${CONFIGDIR}'

# MAKECLEAN: The make to clean up with
MAKECLEAN = make NODEPEND=true SUBCONFIGDIR='${CONFIGDIR}'


###
# Compiler definitions
###

##
# Variables
##

# PPCODE: The preprocessor code
PPCODE ?= ${LIBDIR}/tpquote.cmo

# PPOPTIONS: Preprocessor options 
# To avoid using the preprocessor, 
# set "PPOPTIONS =" (i.e. an empty value)
ifneq (($strip ${PPOPTIONS}), "")
PPOPTIONS ?= -pp "camlp4o q_MLast.cmo ${PPCODE} pa_extend.cmo" -I +camlp4
endif

# OPTIONS: Compiler options
OPTIONS = ${PPOPTIONS}

# INCLUDE: The include directives
INCLUDE = -I ${HEADERSDIR}

##
# Compiler Commands
##

OCAMLC=ocamlc ${OPTIONS} 
OCAMLMKTOP = ocamlmktop ${INCLUDE} ${OCINCLUDE} ${OCLIBS}
OCAMLlink=${OCAMLC} -c ${INCLUDE} ${OCINCLUDE} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${LIB} ${OCINCLUDE} ${OCLIBS} 

###
# The build targets
###

all: code

.PHONY: code
code: 
	$(foreach subdir, ${SUBDIRS}, $(DMAKE) -C ${subdir} install;)

.PHONY: doc
doc:
	$(foreach subdir, ${SUBDIRS}, $(DMAKE) -C ${subdir} doc;)


##
# Executables
##

mkhseq: mkhseq.ml
	${OCAMLC} -o mkhseq mkhseq.ml

hseq: code
	${OCAMLMKTOP} -o ${BINFILE} ${CODE} 

install: hseq
	cp ${BINFILE} ${IBINDIR}/${IBINFILE}
	mkdir ${IBASEDIR};
	mkdir ${ILIBDIR}; cp -R ${LIBDIR} ${ILIBDIR}
	mkdir ${ITHYDIR}; cp -R ${THYDIR} ${ITHYDIR}

###
# Clean up
###

.PHONY: clean
clean: 
	rm -f *.cmo
	rm -f *.cmx  *.o
	rm -f *~
	$(foreach subdir, ${SUBDIRS}, $(MAKECLEAN) -C ${subdir} clean; )

.PHONY: libclean
libclean: 
	rm -f *.cmi
	rm -f *.cma *.cmxa
	$(foreach subdir, ${SUBDIRS}, $(MAKECLEAN) -C ${subdir} libclean; )
	$(MAKECLEAN) -C ${LIBDIR} libclean

.PHONY: reallyclean
reallyclean: 
	rm -f hseq
	rm -f depend
	$(foreach subdir, ${SUBDIRS}, $(MAKECLEAN) -C ${subdir} reallyclean; )
	$(MAKECLEAN) -C ${LIBDIR} reallyclean


.PHONY: binclean
binclean:
	cd bin; rm -f *

.PHONY: docclean
docclean: 
	$(BAREMAKE) -C doc reallyclean
