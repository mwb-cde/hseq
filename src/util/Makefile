# Header, code and optimised code files

HEADERS = lib.cmi unsafe.cmi treekit.cmi tag.cmi parserkit.cmi
CODE = lib.cmo unsafe.cmo treekit.cmo tag.cmo parserkit.cmo
OPTCODE = lib.cmx unsafe.cmx treekit.cmx tag.cmx parserkit.cmx

DOCFILES = lib.mli unsafe.mli treekit.mli tag.mli parserkit.mli
DOCLOAD = 
DOCTYPE ?= -html
DOCDIR ?= "../doc"

LIBFILE=util.cma
OPTLIB=util.cmxa

# Compiler setings

INCLUDE= -I .. -I ../include
LIBDIR= 

HEADERSDIR=../include
INSTALLDIR=../libs
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
	ocamldoc ${DOCLOAD} -dump "${DOCDIR}/util.data" \
	${INCLUDE} ${DOCOPTIONS} -d ${DOCDIR} ${DOCFILES}

# libraries

${LIBFILE}: ${HEADERS} ${CODE}
		${OCAMLC} -o ${LIBFILE} -a ${INCLUDE} ${CODE}

# optimised libraries

${OPTLIB}: ${HEADERS} ${OPTCODE}
		ocamlopt -o ${OPTLIB} -a ${INCLUDE} ${OPTCODE}

opt: ${OPTLIB}

depend: 
	ocamldep  *.ml* > depend

.PHONY: clean
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
