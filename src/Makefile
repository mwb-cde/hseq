SHELL=/bin/sh
export INCLUDE= -I ../include -I ./include
export LIB= -I ./libs -I ../libs -I +camlp4

export SRCDIR=$(shell pwd)

export OCLIBS=nums.cma unix.cma camlp4o.cma pa_extend.cmo q_MLast.cmo
export OPTLIBS=nums.cmxa unix.cmxa
export OPTIONS=-pp "camlp4o q_MLast.cmo ${SRCDIR}/libs/tpquote.cmo pa_extend.cmo" -I +camlp4

INSTALLDIR=${HOME}/bin

OCAMLC=ocamlc ${OPTIONS} 

BAREMAKE=make
MAKE=make INCLUDE='${INCLUDE}' LIB='${LIB}' OPTIONS='${OPTIONS}'


OCAMLlink=${OCAMLC} -custom -c ${INCLUDE} ${LIB} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${LIB} ${OCLIBS} 

BINFILE=fmcaml
OPTFILE=fmcaml.opt

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

CODE= ${QUOTECODE} ${UTILCODE} ${TPCORECODE} ${LOGICCODE} \
	${PARSERCODE}  ${TPMAINCODE} ${SIMPLIFIERCODE} ${NUMBERSCODE}

CODEOPT= ${QUOTECODE} ${UTILOPT} ${TPCOREOPT} ${LOGICOPT} \
	${PARSEROPT} ${TPMAINOPT} ${SIMPLIFIEROPT} ${NUMBERSOPT}

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

# executables

mkfmcaml: mkfmcaml.ml
	${OCAMLC} -o mkfmcaml mkfmcaml.ml

fmcaml: ${CODE} ${OTHER}
	ocamlmktop -custom  -o ${BINFILE} \
	${INCLUDE} ${LIB} ${OCLIBS} ${CODE} ${OTHER}

theories: fmcaml fm.ml build_thys.ml ${THYFILE}
	echo '#use "build_thys.ml";;' | ./fmcaml

install: fmcaml
	mv ${BINFILE} ${INSTALLDIR}/${BINFILE}

THYFILE=base_thy.ml bool_thy.ml bool_prop.ml

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

.PHONY: reallyclean
reallyclean: clean libclean
	rm -f fmcaml
	rm -f depend
	cd quote; $(BAREMAKE) reallyclean
	cd util; $(BAREMAKE) reallyclean
	cd tpcore; $(BAREMAKE) reallyclean
	cd parser; $(BAREMAKE) reallyclean
	cd logic; $(BAREMAKE) reallyclean
	cd tpmain; $(BAREMAKE) reallyclean
	cd numbers; $(BAREMAKE) reallyclean
	cd simplifier; $(BAREMAKE) reallyclean
	cd include; $(BAREMAKE) reallyclean
	cd libs; $(BAREMAKE) reallyclean


.PHONY: commit
commit:
	cd ..; cvs commit src

# dependencies

include depend
