# Header, code and optimised code files

HEADERS = formula.cmi defn.cmi logic.cmi theory.cmi thydb.cmi

CODE = formula.cmo defn.cmo logic.cmo theory.cmo thydb.cmo

OPTCODE = formula.cmx defn.cmx logic.cmx theory.cmx thydb.cmx

DOCFILES = formula.mli defn.mli logic.mli theory.mli thydb.mli
DOCLOAD = -load "${DOCDIR}/tpcore.data"
DOCTYPE ?= -html
DOCDIR ?= "../doc"

LIBFILE=logic.cma
OPTLIB=logic.cmxa

# Compiler setings

INCLUDE= -I .. -I ../include
LIBDIR= 

HEADERSDIR ?=../include
INSTALLDIR ?=../libs
DOCDIR ?= ../doc

OCLIBS=
OPTLIBS=
OPTIONS=-pp "camlp4o pa_extend.cmo" -I +camlp4 

OCAMLC=ocamlc ${OPTIONS} 
OCAMLlink=${OCAMLC} -custom -c ${INCLUDE} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${OCLIBS} 

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


all: ${HEADERS} ${CODE}

lib: ${LIBFILE}

install: lib
	cp -f ${LIBFILE} ${INSTALLDIR}
	cp -f *.cmi *.mli ${HEADERSDIR}

installopt: opt
	cp -f ${OPTLIB} ${INSTALLDIR}
	cp -f *.cmi *.mli ${HEADERSDIR}

doc: ${DOCFILES}
	ocamldoc ${DOCLOAD} -dump "${DOCDIR}/logic.data" \
	${INCLUDE} ${DOCTYPE} ${DOCOPTIONS} -d ${DOCDIR} ${DOCFILES}

# libraries

${LIBFILE}: ${HEADERS} ${CODE}
		${OCAMLC} -o ${LIBFILE} -a ${INCLUDE} ${CODE}

# optimised libraries

${OPTLIB}: ${HEADERS} ${OPTCODE}
		ocamlopt -o ${OPTLIB} -a ${INCLUDE} ${OPTCODE}

opt: ${OPTLIB}

depend: 
	ocamldep  *.ml* > depend

clean:
	rm -f *.cmo
	rm -f *.cmx  *.o
	rm -f *~

libclean: 
	rm -f *.cmi
	rm -f *.cma *.cmxa

reallyclean: clean libclean
	rm -f depend

# dependencies

include depend
