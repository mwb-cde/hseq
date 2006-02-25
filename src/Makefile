##
# Directories
##

# PGKDIR: The toplevel package directory
PGKDIR ?= /home/mw/src/tp

# SRCDIR: The source directory
SRCDIR ?= ${PGKDIR}/src

# CONFIGDIR: The configuration directory
CONFIGDIR ?= ${SRCDIR}/config

# HEADERSDIR: The location of the compiled header files
HEADERSDIR ?= ${SRCDIR}/libs

# LIBDIR: The location of the compiled files
LIBDIR ?= ${SRCDIR}/libs

# DOCDIR: Source code documentation directory
DOCDIR ?= ${SRCDIR}/doc

##
# Usefull definitions
##

SHELL=/bin/sh


##
# Compiler definitions
##

# PPCODE: The preprocessor code
PPCODE ?= ${LIBDIR}/tpquote.cmo

# PPOPTIONS: Preprocessor options 
# To avoid using the preprocessor, 
# set "PPOPTIONS =" (i.e. an empty value)
ifneq (($strip ${PPOPTIONS}), "")
PPOPTIONS ?= -pp "camlp4o q_MLast.cmo ${PPCODE} pa_extend.cmo" -I +camlp4
endif

# OPTIONS: The compiler options
OPTIONS = ${PPOPTIONS}

# INCLUDE: The include directives
INCLUDE = -I ${HEADERSDIR}

###
# Toplevel definitions
###

## 
# Variables
##

INSTALLDIR ?= ./bin

BINFILE = hseq
OPTFILE =

# OCINCLUDE: The required OCaml include directory
OCINCLUDE = -I +camlp4

# OCLIBS: The required OCaml libraries.
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

##
# Commands
##

BAREMAKE = make 
DMAKE = make CONFIGDIR='${CONFIGDIR}'
MAKECLEAN = make NODEPEND=true CONFIGDIR='${CONFIGDIR}'

OCAMLC=ocamlc ${OPTIONS} 
OCAMLMKTOP = ocamlmktop ${INCLUDE} ${OCINCLUDE} ${OCLIBS}
OCAMLlink=${OCAMLC} -c ${INCLUDE} ${OCINCLUDE} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${LIB} ${OCINCLUDE} ${OCLIBS} 

###
# The build targets
###

all: code

code: 
	$(foreach subdir, ${SUBDIRS}, $(DMAKE) -C ${subdir} install;)

srcdoc:
	$(foreach subdir, ${SUBDIRS}, $(DMAKE) -C ${subdir} doc;)


##
# Executables
##

mkhseq: mkhseq.ml
	${OCAMLC} -o mkhseq mkhseq.ml

hseq: code
	${OCAMLMKTOP} -o ${BINFILE} ${CODE} 
#	ocamlmktop -o ${BINFILE} \
#	${INCLUDE} ${OCLIBS} ${CODE}

install: hseq
	cp ${BINFILE} ${INSTALLDIR}/${BINFILE}


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

.PHONY: reallyclean
reallyclean: 
	rm -f hseq
	rm -f depend
	$(foreach subdir, ${SUBDIRS}, $(MAKECLEAN) -C ${subdir} reallyclean; )


.PHONY: binclean
binclean:
	cd bin; rm -f *

.PHONY: docclean
docclean: 
	$(BAREMAKE) -C doc reallyclean
