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
HEADERS = global.cmi drule.cmi tactics.cmi goals.cmi commands.cmi \
	display.cmi boollib.cmi init.cmi

# CODE: the list of code (.cmo) files 
CODE = global.cmo drule.cmo tactics.cmo goals.cmo commands.cmo \
	display.cmo boollib.cmo init.cmo

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = global.cmx drule.cmx tactics.cmx goals.cmx commands.cmx \
	display.cmx boollib.cmx init.cmx

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE = tpmain.cma

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB = tpmain.cmxa

###
# Documentation
###

DOCTYPE = -html

# DOCFILES: File from which documentation is to be built
DOCFILES = global.mli drule.mli tactics.mli goals.mli commands.mli \
	display.mli boollib.mli init.mli

# DOCDATA: File to be emitted as a link
DOCDATA = tpmain.data

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
DOCPREV = parser.data

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
