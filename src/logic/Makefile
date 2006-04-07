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
HEADERS = formula.cmi defn.cmi logic.cmi theory.cmi thydb.cmi

# CODE: the list of code (.cmo) files 
CODE = formula.cmo defn.cmo logic.cmo theory.cmo thydb.cmo

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = formula.cmx defn.cmx logic.cmx theory.cmx thydb.cmx

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE = logic.cma

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB = logic.cmxa

###
# Documentation
###

DOCTYPE = -html

# DOCFILES: File from which documentation is to be built
DOCFILES = formula.mli defn.mli logic.mli theory.mli thydb.mli

# DOCDATA: File to be emitted as a link
DOCDATA = logic.data

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
DOCPREV = tpcore.data

##
# Preprocessor
##
PPOPTIONS = 

###
# End of Makefile.child variables
###


##
## DO NOT MODIFY ANYTHING BELOW THIS POINT
##

ifndef CONFIGDIR
CONFIGDIR = ../config
endif

include $(CONFIGDIR)/Makefile.sub
