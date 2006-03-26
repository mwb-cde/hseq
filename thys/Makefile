SHELL=/bin/sh

#BUILDSCRIPT=setScript.ml
BUILDSCRIPT=buildall.ml

export INCLUDE= -I ../include -I ./include -I ./libs
export LIB= -I +camlp4

export SRCDIR=$(shell pwd)

export OCLIBS=nums.cma unix.cma camlp4o.cma pa_extend.cmo q_MLast.cmo
export OPTLIBS=nums.cmxa unix.cmxa
export OPTIONS=-pp "camlp4o q_MLast.cmo ${SRCDIR}/libs/tpquote.cmo pa_extend.cmo" -I +camlp4

INSTALLDIR ?= ./bin

OCAMLC=ocamlc ${OPTIONS} 

BAREMAKE=make
MAKE=make INCLUDE='${INCLUDE}' LIB='${LIB}' OPTIONS='${OPTIONS}'

OCAMLlink=${OCAMLC} -c ${INCLUDE} ${LIB} ${OCLIBS} 
OCAMLoptlink=ocamlopt -c ${INCLUDE} ${LIB} ${OCLIBS} 

BINFILE=hseq
OPTFILE=hseq.opt

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

all: build

build:
	echo "#use \"${BUILDSCRIPT}\";;" | ../hseq

.PHONY: clean
clean:
	rm -f *.tho
	rm -f *.cmo *.cmi
	rm -f *~

