# Header, code and optimised code files

HEADERS = tpquote.cmi
CODE = tpquote.cmo
LIBFILE = ${CODE}

# Compiler setings

INCLUDE= -I .. -I ../include
LIBDIR= 

HEADERSDIR=../include
INSTALLDIR=../libs

OCLIBS=
OPTLIBS=
OPTIONS=-pp "camlp4o q_MLast.cmo pa_extend.cmo" -I +camlp4 

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

#.ml.cmx: 
#	ocamlopt -c ${INCLUDE} $<

#.cmo.cma:
#	${OCAMLC} -a ${INCLUDE} $<

#.cmx.cmxa:
#	ocamlopt -a ${INCLUDE} $<


all: ${HEADERS} ${CODE}

install: all
	cp -f ${CODE} ${INSTALLDIR}
	cp -f *.cmi *.mli ${HEADERSDIR}


clean:
	rm -f *.cmo
	rm -f *.cmx  *.o
	rm -f *~

libclean: clean
	rm -f *.cmi
	rm -f *.cma *.cmxa

reallyclean: clean libclean

# dependencies

#include depend
