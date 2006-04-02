#####
# Makefile for building sub-directories
#
# Should be called from an instance of Makefile.child
#####

# Load the OS commands
# include Makefile.os

####
# The variables to be set in Makefile.child
###

###
# texi documentation source files
###

# DOCSRC: The source files to be compiled (root file names)
DOCSRC ?= hseq

###
# Generated files and directories
###

# OUTFILES: Generated files, to be moved
OUTFILES ?= 

# OUTDIR: Name of the directory in which to put the generated files
OUTDIR ?= 

###
# End of Makefile.child variables
###

###
# Useful definitions
###

# DIRSEP: The seperator for directories
# On Unix DIRSEP=/, on windows DIRSEP=\
DIRSEP ?= /

##
# Directories
##

# SRCDIR: The source directory (relative to a sub-directory).
SRCDIR ?= ..

# CONFIGDIR: The configuration directory
CONFIGDIR ?= ${SRCDIR}/config

# DOCDIR: Documentation directory
DOCDIR ?= ${SRCDIR}

# HTMLDIR: HTML Documentation directory
HTMLDIR ?= ${DOCDIR}/html

# INFODIR: Info Documentation directory
INFODIR ?= ${DOCDIR}/info

##
# Compiler definitions
##

# INCLUDE: The include directives
INCLUDE = 

# OPTIONS: The compiler options
OPTIONS = ${INCLUDE}

##
# The compilers
##

# MAKEINFO: The makeinfo program
MAKEINFO=makeinfo ${OPTIONS}

###
# Suffixes and how to handle them
###

###
# Build targets
###

# all: Build html
all: html

# html: Build html documentation
.PHONY: html
html: 
	$(foreach file, $(DOCSRC), -$(MAKEINFO) --html $(file).texi;)

# info: Build info documentation
.PHONY: info
info: 
	$(foreach file, $(DOCSRC), -$(MAKEINFO) $(file).texi;)

# install: Install documentation
install: install-html install-info

# install-html: Install html documentation
install-html: html
	-mkdir $(HTMLDIR)
	$(foreach file, $(DOCSRC), cp -Rf $(file) $(HTMLDIR);)

# install-info: Install html documentation
install-info: mkinfo
	-mkdir $(INFODIR)
	cp -Rf *.info $(INFODIR)

##
# Cleaning files
##

clean:
	$(foreach file, $(DOCSRC), rm -rf $(file);)
	rm -f *.info
	rm -f *~

reallyclean: clean 
	rm -f depend

##
# Dependencies
#
# Define NODEPEND to prevent using depend
##

#depend: 
#	${OCAMLDEP} *.mli *.ml > depend

#ifndef NODEPEND
#include depend
#endif

