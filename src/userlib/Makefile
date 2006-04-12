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
HEADERS = userlib.cmi cli.cmi 

# CODE: the list of code (.cmo) files 
CODE = userlib.cmo cli.cmo

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = userlib.cmx cli.cmx

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE = userlib.cma

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB = userlib.cmxa

###
# Documentation
###

DOCTYPE = -html

# DOCFILES: File from which documentation is to be built
DOCFILES = userlib.mli cli.mli

# DOCDATA: File to be emitted as a link
DOCDATA = userlib.data

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
DOCPREV = simplifier.data


###
# End of Makefile.child variables
###


##
## DO NOT MODIFY ANYTHING BELOW THIS POINT
##

ifndef CONFIGDIR
CONFIGDIR = ../config
endif

#include $(CONFIGDIR)/Makefile.sub
