INCLUDE= -I .. -I ../include
LIBDIR= 

HEADERSDIR=../include
INSTALLDIR=../libs

OCLIBS=-I +camlp4 camlp4o.cma q_MLast.cmo pa_extend.cmo 
OPTLIBS=
OPTIONS=-pp "camlp4o q_MLast.cmo pa_extend.cmo" -I +camlp4

OCAMLC=ocamlc ${OPTIONS} 
OCAMLlink=${OCAMLC} -custom -c ${INCLUDE} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${OCLIBS} 

LIBFILE=parser.cma
OPTLIB=parser.cmxa

DOCFILES = lexer.mli parser.mli 
DOCLOAD = -load "${DOCDIR}/logic.data"
DOCTYPE ?= -html
DOCDIR ?= "../doc"

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

HEADERS = counter.cmi lexer.cmi parser.cmi
CODE = counter.cmo lexer.cmo parser.cmo 
OPTCODE = counter.cmx lexers.cmx termlex.cmx termlexer.cmx parsers.cmx tparser.cmx

all: ${HEADERS} ${CODE}

lib: ${LIBFILE}

install: lib
	cp -f ${LIBFILE} ${INSTALLDIR}
	cp -f *.cmi *.mli ${HEADERSDIR}

installopt: opt
	cp -f ${OPTLIB} ${INSTALLDIR}
	cp -f *.cmi *.mli ${HEADERSDIR}

doc: ${DOCFILES}
	ocamldoc ${DOCLOAD} -dump "${DOCDIR}/parser.data" \
	${INCLUDE} ${DOCTYPE} ${DOCOPTIONS} -d ${DOCDIR} ${DOCFILES}

# libraries

${LIBFILE}: ${HEADERS} ${CODE}
		${OCAMLC} -o parser.cma -a ${INCLUDE} ${CODE}

# optimised libraries

${OPTLIB}: ${HEADERS} ${OPTCODE}
		ocamlopt -o parser.cmxa -a ${INCLUDE} ${OPTCODE}

opt: ${OPTLIB}

printer: printer.cmi printer.cmo

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
