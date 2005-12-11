# Header, code and optimised code files

HEADERS = global.cmi drule.cmi tactics.cmi goals.cmi commands.cmi \
	display.cmi boollib.cmi init.cmi
CODE = global.cmo drule.cmo tactics.cmo goals.cmo commands.cmo \
	display.cmo boollib.cmo init.cmo
OPTCODE = global.cmx drule.cmx tactics.cmx goals.cmx commands.cmx \
	display.cmx boollib.cmx init.cmx

LIBFILE=tpmain.cma
OPTLIB=tpmain.cmxa

DOCFILES = global.mli drule.mli tactics.mli goals.mli commands.mli \
	display.mli boollib.mli init.mli
DOCLOAD = -load "${DOCDIR}/parser.data"
DOCTYPE ?= -html
DOCDIR ?= "../doc"

# Compiler setings

INCLUDE= -I .. -I ../include
LIBDIR= 

HEADERSDIR=../include
INSTALLDIR=../libs

SRCDIR=..
OCLIBS=
OPTLIBS=
OPTIONS=-pp "camlp4o q_MLast.cmo ${SRCDIR}/libs/tpquote.cmo pa_extend.cmo" -I +camlp4

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
	ocamldoc ${DOCLOAD} -dump "${DOCDIR}/tpmain.data" \
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
