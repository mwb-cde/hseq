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

# DOCSRC: The source files to be compiled
DOCSRC ?= hseq-quickstart

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

# DOC_DIR: Documentation directory
DOC_DIR?=..

# HTMLDIR: HTML Documentation directory
HTMLDIR?=${DOC_DIR}/html

# INFODIR: Info Documentation directory
INFODIR?=${DOC_DIR}/info

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
MAKEINFO?=makeinfo ${OPTIONS}
COPY?=cp
MKDIR?=mkdir -p

###
# Suffixes and how to handle them
###

###
# Build targets
###

# all: Build html
all: build-html
info: build-info
html: build-html

# html: Build html documentation
.PHONY: build-html

$(HTMLDIR):
	$(MKDIR) $(HTMLDIR)

build-html: $(foreach file,$(DOCSRC),$(file).texi) $(HTMLDIR)
	$(foreach file, $(DOCSRC), \
	$(MAKEINFO) --html $(file).texi; \
	$(COPY) -R $(file) $(HTMLDIR))


# info: Build info documentation
$(INFODIR):
	$(MKDIR) $(INFODIR)

.PHONY: build-info
build-info: $(foreach file,$(DOCSRC),$(file).texi) $(INFODIR)
	$(foreach file, $(DOCSRC), \
	$(MAKEINFO) $(file).texi;\
	$(COPY) -r $(file)*.info $(INFODIR))

# install: Install documentation
install: install-html install-info

# install-html: Install html documentation
install-html: html
	-mkdir $(HTMLDIR)
	$(foreach file, $(DOCSRC), -cp -Rf $(file) $(HTMLDIR);)


# install-info: Install html documentation
install-info: info
	-mkdir $(INFODIR)
	-cp -Rf *.info $(INFODIR)

##
# Cleaning files
##

clean:
	$(foreach file, $(DOCSRC), rm -rf $(file);)
	rm -f *.info
	rm -f *~

reallyclean: clean 
	rm -f depend

