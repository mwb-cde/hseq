# Header, code and optimised code files

HEADERS = settings.cmi basic.cmi corepp.cmi result.cmi \
	gtypes.cmi term.cmi logicterm.cmi dbterm.cmi \
	termnet.cmi \
	typing.cmi unify.cmi net.cmi rewrite.cmi 

CODE =  settings.cmo basic.cmo corepp.cmo result.cmo \
	gtypes.cmo term.cmo logicterm.cmo dbterm.cmo \
	termnet.cmo \
	typing.cmo unify.cmo net.cmo rewrite.cmo 


OPTCODE = settings.cmx basic.cmx corepp.cmx result.cmx \
	gtypes.cmx term.cmx logicterm.cmx dbterm.cmx \
	typing.cmx unify.cmx net.cmx rewrite.cmx 


LIBFILE=tpcore.cma
OPTLIB=tpcore.cmxa

# Compiler setings

INCLUDE= -I .. -I ../include
LIBDIR= 

HEADERSDIR=../include
INSTALLDIR=../libs

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
