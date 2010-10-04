#####
# Makefile for building the quotation system
#
# Include Makefile.def and then Makefile.quote
#####

-include Makefile.def

ifndef SRCCONFIGDIR
SRCCONFIGDIR = ../config
endif

##
# Preprocessor options
## 

# Use the camlp4 version to select the correct 
# prepropcessor options.
ifeq ($(OLDCAMLP4),true)
PPOPTIONS=-pp "camlp4o q_MLast.cmo pa_extend.cmo" -I +camlp4 
else
PPOPTIONS=-pp "camlp4of" -I +camlp4
endif

# Don't try to build native code 
NATIVE := "false"

# Don't use the default dependency generator
NODEPEND = "true"

include $(SRCCONFIGDIR)/Makefile.sub

tpquote.cmi: tpquote.mli
tpquote.cmo: tpquote.cmi tpquote.ml


