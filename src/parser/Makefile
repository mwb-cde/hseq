INCLUDE= -I .. -I ../include 
LIBDIR= 

HEADERSDIR=../include
INSTALLDIR=../libs

OCLIBS=
OPTLIBS=
#OPTIONS=-pp "camlp4o pa_extend.cmo" -I +camlp4 
OPTIONS=

OCAMLC=ocamlc ${OPTIONS} 
OCAMLlink=${OCAMLC} -custom -c ${INCLUDE} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${OCLIBS} 

LIBFILE=parser.cma
OPTLIB=parser.cmxa

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

HEADERS = parserkit.cmi lexer.cmi parser.cmi
CODE = parserkit.cmo lexer.cmo parser.cmo
OPTCODE = lexers.cmx termlex.cmx termlexer.cmx parsers.cmx tparser.cmx

all: ${HEADERS} ${CODE}

lib: ${LIBFILE}

install: lib
	cp -f ${LIBFILE} ${INSTALLDIR}
	cp -f *.cmi *.mli ${HEADERSDIR}

installopt: opt
	cp -f ${OPTLIB} ${INSTALLDIR}
	cp -f *.cmi *.mli ${HEADERSDIR}

# libraries

${LIBFILE}: ${HEADERS} ${CODE}
		${OCAMLC} -o parser.cma -a ${INCLUDE} ${CODE}

# optimised libraries

${OPTLIB}: ${HEADERS} ${OPTCODE}
		ocamlopt -o parser.cmxa -a ${INCLUDE} ${OPTCODE}

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
