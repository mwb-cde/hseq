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
HEADERS = prop.cmi exprs.cmi supinf.cmi nums.cmi

# CODE: the list of code (.cmo) files 
CODE = prop.cmo exprs.cmo supinf.cmo nums.cmo

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = prop.cmx exprs.cmx supinf.cmx nums.cmx

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE = numbers.cma

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB = numbers.cmxa

###
# Documentation
###

DOCTYPE = -html

# DOCFILES: File from which documentation is to be built
DOCFILES =

# DOCDATA: File to be emitted as a link
DOCDATA = 

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
DOCPREV = 


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
