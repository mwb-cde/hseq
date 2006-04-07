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
HEADERS = defaults.cmi settings.cmi ident.cmi basic.cmi printer.cmi \
	result.cmi scope.cmi gtypes.cmi term.cmi logicterm.cmi \
	dbterm.cmi typing.cmi unify.cmi net.cmi rewrite.cmi 

# CODE: the list of code (.cmo) files 
CODE = defaults.cmo settings.cmo ident.cmo basic.cmo printer.cmo \
	result.cmo scope.cmo gtypes.cmo term.cmo logicterm.cmo \
	dbterm.cmo net.cmo typing.cmo unify.cmo net.cmo rewrite.cmo 

# OPTCODE: the list of optimised code (.cmx) files 
OPTCODE = settings.cmx ident.cmx basic.cmx printer.cmx result.cmx \
	scope.cmx gtypes.cmx term.cmx logicterm.cmx dbterm.cmx \
	typing.cmx unify.cmx net.cmx rewrite.cmx 

###
# Generated libraries
###

# LIBFILE: The list of library (.cma) files built
LIBFILE = tpcore.cma

# OPTLIB: The list of optimised library (.cmxa) files built
OPTLIB = tpcore.cmxa

###
# Documentation
###

DOCTYPE = -html

# DOCFILES: File from which documentation is to be built
DOCFILES = defaults.mli settings.mli ident.mli basic.mli \
	printer.mli result.mli scope.mli \
	gtypes.mli term.mli logicterm.mli dbterm.mli \
	net.mli typing.mli unify.mli rewrite.mli 

# DOCDATA: File to be emitted as a link
DOCDATA = tpcore.data

# DOCPREV: File to be loaded as a link to previous documentation
# (using -load)
DOCPREV = util.data

##
# Other variables
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
