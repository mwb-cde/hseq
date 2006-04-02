# Tools

TOOLS = hseqc hseqd

# Compiler settings

INSTALLDIR ?=../bin
CONFIGDIR ?= ../config

SRCDIR=?.
OCLIBS=
OPTLIBS=
OPTIONS=

OCAMLC=ocamlc ${OPTIONS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${OCLIBS} 
OCAMLCPP = camlp4 pa_o.cmo pr_o.cmo pa_extend.cmo pa_macro.cmo -- -I ${CONFIGDIR} 

.SUFFIXES: .mli .cmi .mlp
.SUFFIXES: .ml .cmo
.SUFFIXES: .cma 
.SUFFIXES: .cmx 
.SUFFIXES: .cmxa 

.mli.cmi: 
	${OCAMLC} -c ${INCLUDE} $<

.ml.cmo: 
	${OCAMLC} -c ${INCLUDE} $<

.mlp.ml:
	${OCAMLCPP} -impl $< -o $@

.ml.cmx: 
	ocamlopt -c ${INCLUDE} $<

.cmo.cma:
	${OCAMLC} -a ${INCLUDE} $<

.cmx.cmxa:
	ocamlopt -a ${INCLUDE} $<

#
# Built sources
# file built as a side-effect
#
BUILT = hseqc.ml

#
# Targets
#
all: ${TOOLS}

hseqc: hseqc.ml 
	${OCAMLC} unix.cma -o hseqc hseqc.ml 
hseqc.ml: hseqc.mlp

hseqd: hseqd.ml 
	${OCAMLC} unix.cma -o hseqd hseqd.ml 
#	cp hseqd.ml hseqd
# hseqd.ml: hseqd.mlp

install: ${TOOLS}
	cp -f ${TOOLS} ${INSTALLDIR}

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

clean:
	rm -f ${TOOLS} ${BUILT}
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
