SHELL=/bin/sh
export INCLUDE= -I ../include -I ./include -I ./libs
#export LIB= -I ./libs -I ../libs -I +camlp4
export LIB= -I +camlp4

export SRCDIR=$(shell pwd)

export OCLIBS=nums.cma unix.cma camlp4o.cma pa_extend.cmo q_MLast.cmo
export OPTLIBS=nums.cmxa unix.cmxa
export OPTIONS=-pp "camlp4o q_MLast.cmo ${SRCDIR}/libs/tpquote.cmo pa_extend.cmo" -I +camlp4

INSTALLDIR ?= ./bin

OCAMLC=ocamlc ${OPTIONS} 

BAREMAKE=make
MAKE=make INCLUDE='${INCLUDE}' LIB='${LIB}' OPTIONS='${OPTIONS}'


#OCAMLlink=${OCAMLC} -custom -c ${INCLUDE} ${LIB} ${OCLIBS} 
OCAMLlink=${OCAMLC} -c ${INCLUDE} ${LIB} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${LIB} ${OCLIBS} 

BINFILE=hseq
OPTFILE=hseq.opt

.SUFFIXES: .mli .cmi
.SUFFIXES: .ml .cmo
.SUFFIXES: .cma 
.SUFFIXES: .cmx 
.SUFFIXES: .cmxa 

.mli.cmi: 
	${OCAMLC} -c ${INCLUDE} $<

.ml.cmo: 
	${OCAMLC} -c ${INCLUDE} $<

.ml.cmx: 
	ocamlopt -c ${INCLUDE} $<

.cmo.cma:
	${OCAMLC} -a ${INCLUDE} $<

.cmx.cmxa:
	ocamlopt -a ${INCLUDE} $<

QUOTEHD=tpquote.cmi
QUOTECODE=${SRCDIR}/libs/tpquote.cmo

UTILCODE=util.cma
UTILOPT=util.cmxa

TPCORECODE=tpcore.cma
TPCOREOPT=tpcore.cmxa

LOGICCODE = logic.cma
LOGICOPT = logic.cmxa

PARSERCODE = parser.cma
PARSEROPT = parser.cmxa

TPMAINCODE=tpmain.cma
TPMAINOPT=tpmain.cmxa

SIMPLIFIERCODE=simplifier.cma
SIMPLIFIEROPT=simplifier.cmxa

NUMBERSCODE=numbers.cma
NUMBERSOPT=numbers.cmxa

USERLIBCODE=userlib.cma
USERLIBOPT=userlib.cmxa


CODE= ${QUOTECODE} ${UTILCODE} ${TPCORECODE} ${LOGICCODE} \
	${PARSERCODE}  ${TPMAINCODE} ${SIMPLIFIERCODE} ${NUMBERSCODE} \
	${USERLIBCODE}

CODEOPT= ${QUOTECODE} ${UTILOPT} ${TPCOREOPT} ${LOGICOPT} \
	${PARSEROPT} ${TPMAINOPT} ${SIMPLIFIEROPT} ${NUMBERSOPT} \
	${USERLIBOPT}

OTHER=


all: ${CODE} ${OTHER}


# the quotation system

${QUOTECODE}:
	cd quote; make install

# headers and libraries

${UTILCODE}: 
	cd util; $(MAKE) install

${TPCORECODE}: 
	cd tpcore; $(MAKE) install

${PARSERCODE}: ${TPCORECODE}
	cd parser; $(MAKE) install

${LOGICCODE}: ${TPCORECODE}
	cd logic; $(MAKE) install

${TPMAINCODE}: ${TPCORECODE} ${PARSERCODE}
	cd tpmain; $(MAKE) install

${SIMPLIFIERCODE}: ${TPMAINCODE}
	cd simplifier; $(MAKE) install

${NUMBERSCODE}: ${TPMAINCODE}
	cd numbers; $(MAKE) install

${USERLIBCODE}: ${TPMAINCODE}
	cd userlib; $(MAKE) install

# optimised libraries

${UTILOPT}:
	cd util; $(MAKE) opt

${TPCOREOPT}:
	cd tpcore; $(MAKE) opt

${LOGICOPT}: 
	cd logic; $(MAKE) opt

${PARSEROPT}: ${TPCOREOPT} ${TPCOREOPT}
	cd parser; $(MAKE) opt

${TPMAINOPT}: ${TPCOREOPT} ${PARSEROPT} 
	cd tpmain; $(MAKE) opt

${SIMPLIFIEROPT}: ${TPMAINOPT}
	cd simplifier; $(MAKE) opt

${NUMBERSOPT}: ${TPMAINOPT}
	cd main; $(MAKE) opt

${USERLIBOPT}: ${TPMAINOPT}
	cd userlib; $(MAKE) opt

# executables

mkhseq: mkhseq.ml
	${OCAMLC} -o mkhseq mkhseq.ml

hseq: ${CODE} ${OTHER}
	ocamlmktop  -o ${BINFILE} \
	${INCLUDE} ${LIB} ${OCLIBS} ${CODE} ${OTHER}

theories: hseq fm.ml build_thys.ml ${THYFILE}
	echo '#use "build_thys.ml";;' | ./hseq

install: hseq
	cp ${BINFILE} ${INSTALLDIR}/${BINFILE}

THYFILE=base_thy.ml bool_thy.ml bool_prop.ml

###
# Documentation
###

DOCDIR ?= ../doc
DOCOPTIONS ?= -html
MAKEDOC = make DOCOPTIONS='${DOCOPTIONS}' DOCDIRS='${DOCDIR}'

#DOC= ${QUOTEDOC} ${UTILDOC} ${TPCOREDOC} ${LOGICDOC} \
#	${PARSERDOC}  ${TPMAINDOC} ${SIMPLIFIERDOC} ${NUMBERSDOC} \
#	${USERLIBDOC}

DOC= tpcoredoc

doc: ${DOC}

# the quotation system

quotedoc:
	cd quote; $(MAKEDOC) doc

# headers and libraries

utildoc: 
	cd util; $(MAKEDOC) doc

tpcoredoc: utildoc
	cd tpcore; $(MAKEDOC) doc

parserdoc: tpcoredoc
	cd parser; $(MAKEDOC) doc

logicdoc: tpcoredoc
	cd logic; $(MAKEDOC) doc

tpmaindoc: parserdoc logicdoc
	cd tpmain; $(MAKEDOC) doc

simplifierdoc: tpmaindoc
	cd simplifier; $(MAKEDOC) doc

numbersdoc: tpmaindoc
	cd numbers; $(MAKEDOC) doc

userlibdoc: tpmaindoc
	cd userlib; $(MAKEDOC) doc


depend: 
	ocamldep  *.ml* > depend

.PHONY: clean
clean:
	rm -f *.cmo
	rm -f *.cmx  *.o
	rm -f *~
	cd include; $(BAREMAKE) clean
	cd quote; $(BAREMAKE) clean
	cd util; $(BAREMAKE) clean
	cd tpcore; $(BAREMAKE) clean
	cd parser; $(BAREMAKE) clean
	cd logic; $(BAREMAKE) clean
	cd tpmain; $(BAREMAKE) clean
	cd simplifier; $(BAREMAKE) clean
	cd numbers; $(BAREMAKE) clean

.PHONY: libclean
libclean: clean
	rm -f *.cmi
	rm -f *.cma *.cmxa
	cd include; $(BAREMAKE) libclean
	cd libs; $(BAREMAKE) libclean
	cd doc; $(BAREMAKE) libclean

.PHONY: reallyclean
reallyclean: clean libclean
	rm -f hseq
	rm -f depend
	cd quote; $(BAREMAKE) reallyclean
	cd util; $(BAREMAKE) reallyclean
	cd tpcore; $(BAREMAKE) reallyclean
	cd parser; $(BAREMAKE) reallyclean
	cd logic; $(BAREMAKE) reallyclean
	cd tpmain; $(BAREMAKE) reallyclean
	cd numbers; $(BAREMAKE) reallyclean
	cd simplifier; $(BAREMAKE) reallyclean
	cd userlib; $(BAREMAKE) reallyclean
	cd include; $(BAREMAKE) reallyclean
	cd libs; $(BAREMAKE) reallyclean
	cd doc; $(BAREMAKE) reallyclean

.PHONY: commit
commit:
	cd ..; cvs commit src

# dependencies

include depend
