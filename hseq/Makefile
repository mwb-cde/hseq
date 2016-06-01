########################################################## --*- Makefile -*--
# Makefile - Makefile to HSeq library
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
# You should have received a copy of the Lesser GNU General Public License
# along with HSeq.  If not see <http://www.gnu.org/licenses/>.
######################################################################

#####
# Definitions and rules for building modules.
#
# REQUIREMENTS
# ------------
#
# Must set PROJ_ROOT.
# PROJ_ROOT: Variable containing relative path to top of the source tree.
#
# Must end with
#    include $(PROJ_ROOT)/Makefile.common
#
#
# Variables:
# ----------
#
# Module settings:
#
# SUBDIRS: List of sub-directories to recurse into.
# LIBRARIES: List of libraries to build.
# PACKAGES: List of packages to build.
# PROGRAMS: List of programs to build.
#

#####
# Required definitions
#

# PROJ_ROOT: Relative path to HSeq root.
PROJ_ROOT=..

#####
# Module building settings
#

# SUBDIRS: List of sub-directories to build.
#SUBDIRS=util tpcore logic parser tpmain booleans \
#	simplifier numbers

# Packages:List of packages to build.
PACKAGES=HSeq

# PACKED: List of packed libraries to build
#PACKED=hseq

#####
# Object building settings
#

# object_INCLUDE: Directories to include when compiling [object].
#hseq_INCLUDE=util tpcore logic parser tpmain booleans \
#	simplifier numbers

# object_LIBS: Libraries to include when linking [object].
#hseq_LIBS=util/util tpcore/tpcore logic/logic parser/parser \
#	tpmain/tpmain booleans/boollib simplifier/simplifier \
#	numbers/numbers userlib/userlib

#hseq_LIBS=util tpcore logic parser \
#	tpmain boollib simplifier \
#	numbers

# object_PACK_NAME: Name of pack to build
#object_PACK_NAME=
#hseq_PACK_NAME=HSeq

# object_PACK_DIR: Directory to use for packing.
#object_PACK_DIR=
#hseq_PACK_DIR=./HSeqOBJS

# object_GROUPS: List of packing groups to use for packed library [object]
#hseq_GROUPS=util tpcore logic parser \
#	tpmain boollib simplifier \
#	numbers


# Package HSeq

#HSeq_INCLUDE=.

HSeq_LIBRARY=hseq

HSeq_PACK_NAME=HSeq

# HSeq sources
MODULE_OCAMLPP_FLAGS=-pp "$(CAMLPO) q_MLast.cmo $(TPQUOTELIB) pa_extend.cmo" -I $(CAMLPLIB)

Util_SOURCES=lib treekit tag parserkit rewritekit
Tpcore_SOURCES=defaults settings ident basic printer \
		report scope gtypes term lterm \
		dbterm typing unify net rewrite
Logic_SOURCES=changes formula defn subgoalkit logic theory thydb
Parser_SOURCES=counter lexer pterm grammars parser
Tpmain_SOURCES=context boolPP tactics goals commands display
Booleans_SOURCES=baseTheory boolutil boolbase \
		rewritelib booltacs boolinduct boollib
Numbers_SOURCES=prop exprs supinf nums
Simplifier_SOURCES=simputils simpconvs simpset simplifier \
		   simptacs simplib

HSeq_SOURCES=$(Util_SOURCES) $(Tpcore_SOURCES) 	$(Logic_SOURCES) \
		$(Parser_SOURCES) $(Tpmain_SOURCES) $(Booleans_SOURCES) \
		$(Numbers_SOURCES) $(Simplifier_SOURCES)

######################################################################
# DO NOT CHANGE ANYTHING BELOW THIS LINE
######################################################################

###
# Include common definitions

# Test for PROJ_ROOT being defined.
ifndef PROJ_ROOT
$(error "PROJ_ROOT Must be set to relative path to HSeq root")
endif

include $(PROJ_ROOT)/Makefile.rules
