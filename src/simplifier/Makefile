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
HEADERS = simputils.cmi simpconvs.cmi simpset.cmi simplifier.cmi \
	simptacs.cmi simplib.cmi

# CODE: the list of code (.cmo) files 
CODE = simputils.cmo simpconvs.cmo simpset.cmo simplifier.cmo \
	simptacs.cmo simplib.cmo

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = simputils.cmx simpset.cmx simpconvs.cmo simplifier.cmx \
	simptacs.cmx simplib.cmx

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE= simplifier.cma

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB= simplifier.cmxa

###
# Documentation
###

# DOCFILES: File from which documentation is to be built
DOCFILES = simputils.mli simpconvs.mli simpset.mli simplifier.mli \
	simptacs.mli simplib.mli

# DOCDATA: File to be emitted as a link
DOCDATA = simplifier.data

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
DOCPREV = tpmain.data


###
# End of Makefile.child variables
###


##
## DO NOT MODIFY ANYTHING BELOW THIS POINT
##

ifndef CONFIGDIR
CONFIGDIR = ../config
endif

include ${CONFIGDIR}/Makefile.sub

