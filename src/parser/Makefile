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
HEADERS = counter.cmi lexer.cmi parser.cmi

# CODE: the list of code (.cmo) files 
CODE = counter.cmo lexer.cmo parser.cmo 

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = counter.cmx lexer.cmx parser.cmx 

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE = parser.cma

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB = parser.cmxa

###
# Documentation
###

DOCTYPE = -html

# DOCFILES: File from which documentation is to be built
DOCFILES = lexer.mli parser.mli 

# DOCDATA: File to be emitted as a link
DOCDATA = parser.data

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
DOCPREV = logic.data


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
