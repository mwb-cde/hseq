#####
# Makefile for building the quotation system
#
# Include Makefile.def and then Makefile.quote
#####

-include Makefile.def

ifndef CONFIGDIR
CONFIGDIR = ../config
endif

##
# Preprocessor options
## 
PPOPTIONS=-pp "camlp4o q_MLast.cmo pa_extend.cmo" -I +camlp4 

# Don't use the default dependency generator
NODEPEND = "true"

include $(CONFIGDIR)/Makefile.sub

tpquote.cmi: tpquote.mli
tpquote.cmo: tpquote.cmi tpquote.ml

