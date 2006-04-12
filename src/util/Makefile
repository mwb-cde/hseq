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
HEADERS = lib.cmi unsafe.cmi treekit.cmi tag.cmi parserkit.cmi rewritekit.cmi

# CODE: the list of code (.cmo) files 
CODE = lib.cmo unsafe.cmo treekit.cmo tag.cmo parserkit.cmo rewritekit.cmo

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = lib.cmx unsafe.cmx treekit.cmx tag.cmx parserkit.cmx rewritekit.cmx

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE = util.cma

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB = util.cmxa

###
# Documentation
###

DOCTYPE = -html

# DOCFILES: File from which documentation is to be built
DOCFILES = lib.mli unsafe.mli treekit.mli tag.mli parserkit.mli rewritekit.mli

# DOCDATA: File to be emitted as a link
DOCDATA = util.data

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
DOCPREV = 

##
# Clear the PPOPTIONS
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

#include $(CONFIGDIR)/Makefile.sub
