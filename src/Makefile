########################################################## --*- Makefile -*--
# Makefile - Makefile to HSeq library
# Copyright 16 March, 2011, Matthew Wahab <mwb.cde@gmail.com>
#
# Released under the Lesser GPLv3 license:
# ========================================
# This file is part of HSeq.
#
# HSeq is free software; you can redistribute it and/or modify it
# under the terms of the Lesser GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# HSeq is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
# License for more details.
#
# You should have received a copy of the Lesser GNU General Public
# License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
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
# PROGRAMS: List of programs to build.
#
# MODULE_INST_PATH: PATH to append to the relative installation path.
# MODULE_USE_OBJDIR [yes/no]: Whether to use the staging directory.
# MODULE_OCAMLC_FLAGS: Flags to pass to the compiler for [object].
# MODULE_OCAMLPP_FLAGS: Flags to pass to the preprocessor for [object].
# MODULE_LINK_FLAGS: Flags to pass to the linker for [object].
# MODULE_OBJ_PATH: PATH to append to the staging directory path.
# MODULE_INST_PATH: PATH to append to the relative installation path.
#
# For each program or library [object] listed in LIBRARIES or PROGRAMS:
#
# object_SOURCES: List of source file names, without suffixes.
# object_INCLUDE: Directories to include when compiling [object].
# object_LIBS: Libraries to include when linking [object].
# object_OCAMLC_FLAGS: Flags to pass to the compiler for [object].
# object_LINK_FLAGS: Flags to pass to the linker for [object].
# object_INST_PATH: PATH to append to the relative installation path.
#
# Inherited variables:
#
# GLOBAL_OCAMLC_FLAGS: Flags to pass to the compiler for [object].
# GLOBAL_OCAMLPP_FLAGS: Flags to pass to the preprocessor for [object].
# GLOBAL_LINK_FLAGS: Flags to pass to the linker for [object].
# GLOBAL_OBJ_PATH: PATH staging directory path.
# GLOBAL_INST_PATH: PATH installation path.
#
# Other variales
# LOCAL_OCAMLC_FLAGS: Actual flags to pass to the compiler for [object].
# LOCAL_OCAMLPP_FLAGS: flags to pass to the preprocessor for [object].
# LOCAL_LINK_FLAGS: Flags to pass to the linker for [object].
#
# Notes:
#
# The flags for each tool X are formed as 
#    (LOCAL_X_FLAGS) (object_X_FLAGS)
#
# If LOCAL_X_FLAGS is not defined, it defaults to
#    LOCAL_X_FLAGS = (GLOBAL_X_FLAGS) (MODULE_X_FLAGS) 
#
# The global flags for X passed to each sub-directory is
#     (LOCAL_X_FLAGS)
#
# An object will be installed into the directory formed by 
#      $(INST_PATH)/$(MODULE_INST_PATH)/$(OBJECT_INST_PATH)
#
#####

#####
# Required definitions
#  

# PROJ_ROOT: Relative path to HSeq root.
PROJ_ROOT=..

#####
# Module building settings
#

# SUBDIRS: List of sub-directories to build.
SUBDIRS=util tpcore logic parser tpmain booleans \
	simplifier numbers 

# LIBRARIES: List of libraries to build.
#LIBRARIES=hseq

# PROGRAMS: List of programs to build.
#PROGRAMS=

# PACKED: List of packed libraries to build
PACKED=hseq

# PACKING: List of packing modules to build
#PACKING=

#####
# Object building settings
#

# object_INCLUDE: Directories to include when compiling [object].
hseq_INCLUDE=util tpcore logic parser tpmain booleans \
	simplifier numbers 

# object_LIBS: Libraries to include when linking [object].
#hseq_LIBS=util/util tpcore/tpcore logic/logic parser/parser \
#	tpmain/tpmain booleans/boollib simplifier/simplifier \
#	numbers/numbers userlib/userlib

hseq_LIBS=util tpcore logic parser \
	tpmain boollib simplifier \
	numbers 

# object_PACK_NAME: Name of pack to build
#object_PACK_NAME=
hseq_PACK_NAME=HSeq

# object_PACK_DIR: Directory to use for packing.
#object_PACK_DIR=
hseq_PACK_DIR=./HSeqOBJS

# object_GROUPS: List of packing groups to use for packed library [object]
hseq_GROUPS=util tpcore logic parser \
	tpmain boollib simplifier \
	numbers 


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


