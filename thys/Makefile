
########################################################## --*- Makefile -*--
# Makefile.module - Makefile to build modules
# Copyright 2011-2016, Matthew Wahab <mwb.cde@gmail.com>
#
# Released under the Lesser GPLv3 license:
# ========================================
# This file is part of HSeq.
#
# HSeq is free software; you can redistribute it and/or modify it under the
# terms of the Lesser GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# HSeq is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public License for
# more details.
#
# You should have received a copy of the Lesser GNU General Public
# License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
######################################################################

#####
# Definitions and rules for building theories.
#####

#####
# Required definitions
#

# PROJ_ROOT: Relative path to HSeq root.
PROJ_ROOT=..

###
# Variables
###

# BUILDSCRIPT: The name of the build script
BUILDSCRIPT=buildall.ml

# HSEQ_TOOLS: The tools directory for $(HSEQ).
HSEQ_TOOLS=$(SRC_ROOT)/tools

# HSEQ: The name of the theorem prover executable
HSEQ=$(HSEQ_TOOLS)/toplevel/hseq

# HSEQB: The name of the theorem prover scripter
HSEQB=$(HSEQ_TOOLS)/scripts/hseqb

# HSEQLIB: The include directory for $(HSEQ) (before installation).
HSEQLIB=$(SRC_ROOT)/hseq $(SRC_ROOT)/hsequser \
	$(HSEQ_TOOLS)/hseqtop $(HSEQ_TOOLS)/quote \
	../obj/lib

####
# Don't modify anything below this line
####

include $(PROJ_ROOT)/Makefile.rules

###
# Compiler options
###

##
# Variables
##

# INCLUDE: The include directives
HSEQ_INCLUDE=$(foreach dir,$(HSEQLIB),--include $(dir) )

# HSEQC: The hseq compiler
HSEQC=$(HSEQB) --bin $(HSEQ) $(HSEQ_INCLUDE)

###
# Required build targets
###

.PHONY: all
.PHONY: thys
.PHONY: install
.PHONY: install-thys
.PHONY: doc

.PHONY: clean
.PHONY: libclean
.PHONY: distclean

# build: Build everything
CUSTOM_TARGET=build=yes
build: thys

#install: Install everything
install: install-thys

###
# Clean up
###

EXTRA_CLEAN=$(RM) *.tho *.cmo *.cmi *~ local_thy_config.ml

# clean:
# 	$(RM) *.tho *.cmo *.cmi
# 	$(RM) *~

# libclean: clean

# distclean: libclean

###
# Specialized targets
###

# The local configure data.
local_thy_config.ml: Makefile
	echo \
	"#use \"$(SRC_ROOT)/tools/hseqtop/hseqstart.ml\";;\n" \
	"let local_include_dirs = " \
	"[\"$(SRC_ROOT)/hseq\"; \"$(SRC_ROOT)/hsequser\"];;" > \
	local_thy_config.ml

thys: local_thy_config.ml
	$(HSEQC) $(BUILDSCRIPT)

lm-thys-installed-files:=\
	$(foreach file, *.tho *Script.ml *.cmi *.cmo,\
			$(INSTALLDIR_THYS)/$(file) )

.PHONY: install-thys
install-thys: thys $(INSTALLDIR_THYS)
	-$(COPY) *.tho $(INSTALLDIR_THYS)
	-$(COPY) *Script.ml $(INSTALLDIR_THYS)
	-$(COPY) *.cmi $(INSTALLDIR_THYS)
	-$(COPY) *.cmo $(INSTALLDIR_THYS)
	$(CHMOD) $(lm-file-permissions) $(lm-thys-installed-files)
