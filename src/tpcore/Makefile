# Header, code and optimised code files

HEADERS = defaults.cmi settings.cmi basic.cmi printer.cmi result.cmi \
	settings.cmi basic.cmi result.cmi scope.cmi \
	gtypes.cmi term.cmi logicterm.cmi dbterm.cmi \
	net.cmi \
	typing.cmi unify.cmi net.cmi rewrite.cmi 

CODE =  defaults.cmo settings.cmo basic.cmo printer.cmo result.cmo \
	scope.cmo gtypes.cmo term.cmo logicterm.cmo dbterm.cmo \
	net.cmo \
	typing.cmo unify.cmo net.cmo rewrite.cmo 


OPTCODE = settings.cmx basic.cmx printer.cmx result.cmx \
	scope.cmx gtypes.cmx term.cmx logicterm.cmx dbterm.cmx \
	typing.cmx unify.cmx net.cmx rewrite.cmx 

DOCFILES = gtypes.mli term.mli net.mli


LIBFILE=tpcore.cma
OPTLIB=tpcore.cmxa

# Compiler setings

INCLUDE= -I .. -I ../include
LIBDIR= 
CONFIGDIR ?= ../config

HEADERSDIR=../include
INSTALLDIR=../libs

OCLIBS=
OPTLIBS=
OPTIONS=-pp "camlp4o pa_extend.cmo" -I +camlp4 

OCAMLC=ocamlc ${OPTIONS} 
OCAMLlink=${OCAMLC} -custom -c ${INCLUDE} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${OCLIBS} 

# OCAMLCPP : the preprocesor
OCAMLCPP = camlp4 pa_o.cmo pr_o.cmo pa_extend.cmo pa_macro.cmo -- -I ${CONFIGDIR} 

.SUFFIXES: .mli .cmi
.SUFFIXES: .ml .cmo
.SUFFIXES: .cma 
.SUFFIXES: .cmx 
.SUFFIXES: .cmxa 
.SUFFIXES: .mlp

.mli.cmi: 
	${OCAMLC} -c ${INCLUDE} $<

.ml.cmo: 
	${OCAMLC} -c ${INCLUDE} $<

.ml.cmx: 
	ocamlopt -c ${INCLUDE} $<

.mlp.ml:
	${OCAMLCPP} -impl $< -o $@

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
	ocamldoc ${INCLUDE} -html -d ../doc ${DOCFILES}

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
	rm -f defaults.ml
	rm -f *.cmi
	rm -f *.cma *.cmxa

reallyclean: clean libclean
	rm -f depend

# dependencies

defaults.ml: defaults.mlp

include depend
