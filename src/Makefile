SHELL=/bin/sh
export INCLUDE= -I ../include -I ./include
export LIB= -I ./libs -I ../libs -I +camlp4

#LIBS= #-cclib -lnums -cclib -lunix

export SRCDIR=$(shell pwd)

export OCLIBS=nums.cma unix.cma camlp4o.cma pa_extend.cmo q_MLast.cmo
export OPTLIBS=nums.cmxa unix.cmxa
export OPTIONS=-pp "camlp4o q_MLast.cmo ${SRCDIR}/libs/tpquote.cmo pa_extend.cmo" -I +camlp4

INSTALLDIR=${HOME}/bin

OCAMLC=ocamlc ${OPTIONS} 

MAKE=make INCLUDE='${INCLUDE}' LIB='${LIB}' OPTIONS='${OPTIONS}'

OCAMLlink=${OCANMLC} -custom -c ${INCLUDE} ${LIB} ${OCLIBS} 
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

NUMBERSCODE=numbers.cma
NUMBERSOPT=numbers.cmxa

CODE= ${QUOTECODE} ${UTILCODE} ${TPCORECODE} ${LOGICCODE} \
	${PARSERCODE}  ${TPMAINCODE} ${NUMBERSCODE}

CODEOPT= ${QUOTECODE} ${UTILOPT} ${TPCOREOPT} ${LOGICOPT} \
	${PARSEROPT} ${TPMAINOPT} ${NUMBERSOPT}

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
	cd include; $(MAKE) clean
	cd quote; $(MAKE) clean
	cd util; $(MAKE) clean
	cd tpcore; $(MAKE) clean
	cd parser; $(MAKE) clean
	cd logic; $(MAKE) clean
	cd tpmain; $(MAKE) clean
	cd numbers; $(MAKE) clean

.PHONY: libclean
libclean: clean
	rm -f *.cmi
	rm -f *.cma *.cmxa
	cd include; $(MAKE) libclean
	cd libs; $(MAKE) libclean

.PHONY: reallyclean
reallyclean: clean libclean
	rm -f fmcaml
	rm -f depend
	cd quote; $(MAKE) reallyclean
	cd util; $(MAKE) reallyclean
	cd tpcore; $(MAKE) reallyclean
	cd parser; $(MAKE) reallyclean
	cd logic; $(MAKE) reallyclean
	cd tpmain; $(MAKE) reallyclean
	cd numbers; $(MAKE) reallyclean

.PHONY: commit
commit:
	cd ..; cvs commit src

# dependencies

include depend
