#####
# Makefile for building sub-directories
#
# Set variables to be used by Makefile.sub
# This is a template to be copied into each directory to be built.
#####

####
# The variables to be set in Makefile.child
###

###
# Header, code and optimised code files
###

# HEADERS: the list of header (.cmi) files 
HEADERS = tpquote.cmi

# CODE: the list of code (.cmo) files 
CODE = tpquote.cmo

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = 

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE = 

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB = 

###
# Documentation
###

DOCTYPE = -html

# DOCFILES: File from which documentation is to be built
#DOCFILES =

# DOCDATA: File to be emitted as a link
#DOCDATA = 

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
#DOCPREV = 

##
# Preprocessor options
## 
PPOPTIONS=-pp "camlp4o q_MLast.cmo pa_extend.cmo" -I +camlp4 

###
# End of Makefile.child variables
###

##
## DO NOT MODIFY ANYTHING BELOW THIS POINT
##

ifndef CONFIGDIR
CONFIGDIR = ../config
endif

# Don't use the default dependency generator
NODEPEND = "true"

# Custom build and install
CUSTOM_INSTALL = "true"

include $(CONFIGDIR)/Makefile.sub

tpquote.cmi: tpquote.mli
tpquote.cmo: tpquote.cmi tpquote.ml

.PHONY: install
install: $(HEADERS) $(CODE)
	$(COPY) tpquote.cmo $(LIBDIR)
	$(COPY) $(HEADERS) *.mli $(HEADERSDIR)
