########################################################## --*- Makefile -*--
# Makefile.module - Makefile to build modules
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
PROJ_ROOT=../..

#####
# Module building settings
#

# SUBDIRS: List of sub-directories to build.
#SUBDIRS=

# LIBRARIES: List of libraries to build.
#LIBRARIES=tpmain

# PROGRAMS: List of programs to build.
#PROGRAMS=

# PACKING: List of packing modules to build
PACKING=HSeq

#####
# Sub-directory building options

# SUBDIR_MAKE_OPTIONS: Options to pass to the sub-directory make.
#SUBDIR_MAKE_OPTIONS=

#####
# Object building settings
#

# object_SOURCES: List of source file names, without suffixes.
#object_SOURCES=
HSeq_SOURCES=context boolPP tactics goals commands display 

# object_INCLUDE: Directories to include when compiling [object].
#object_INCLUDE=

# object_LIBS: Libraries to include when linking [object].
#object_LIBS=

# object_PACK: Name of packed module to generate (libraries only).
#object_PACK=

# object_PACK_DIR: Directory to use for packing.
#object_PACK_DIR=
HSeq_PACK_DIR=../HSeqOBJS

# object_OCAMLC_FLAGS: Flags to pass to the compiler for [object].
#object_OCAMLC_FLAGS=

# object_OCAMLPP_FLAGS: Flags to pass to the preprocessor for [object].
#object_OCAMLPP_FLAGS=

# object_LINK_FLAGS: Flags to pass to the linker for [object].
#object_LINK_FLAGS=

# object_INST_PATH: PATH to append to the relative installation path.
#object_INST_PATH=

#####
# Module build tool settings

# MODULE_OBJ_PATH: PATH to append to the staging directory path for
# this module and all its sub-directories.
#MODULE_OBJ_PATH=

# MODULE_USE_OBJDIR [yes/no]: Whether to use the staging directory.
#MODULE_USE_OBJDIR=no

# MODULE_OCAMLC_FLAGS: Flags to pass to the compiler for [object].
#MODULE_OCAMLC_FLAGS=

# MODULE_OCAMLPP_FLAGS: Flags to pass to the pre-processor for [object].
#MODULE_OCAMLPP_FLAGS=
MODULE_OCAMLPP_FLAGS=-pp "$(CAMLPO) q_MLast.cmo $(TPQUOTELIB) pa_extend.cmo" -I $(CAMLPLIB)

# MODULE_OCAMLCPP_FLAGS: Flags to pass to the macro-expander.
#MODULE_OCAMLCPP_FLAGS=

# MODULE_LINK_FLAGS: Flags to pass to the linker for [object].
#MODULE_LINK_FLAGS=

###
# Installation options

# For src/* modules, install headers but not libraries.

# INSTALL_LIBRARIES: Libraries to install
INSTALL_LIBRARIES=

# INSTALL_HEADERS: Headers to install
#INSTALL_HEADERS=

###
# Makefile commands

# EXTRA_CLEAN: Command to add to the clean target.
#EXTRA_CLEAN=

# EXTRA_LIBCLEAN: Command to add to the clean target.
#EXTRA_LIBCLEAN=

# EXTRA_DISTCLEAN: Command to add to the clean target.
#EXTRA_DISTCLEAN=


#####
# Default settings

# LOCAL_OCAMLC_FLAGS: Flags to pass to the compiler for [object].
#LOCAL_OCAMLC_FLAGS=$(GLOBAL_OCAMLC_FLAGS) $(MODULE_OCAMLC_FLAGS)

# LOCAL_OCAMLPP_FLAGS: Flags to pass to the pre-processor for [object].
#LOCAL_OCAMLPP_FLAGS=$(GLOBAL_OCAMLPP_FLAGS) $(MODULE_OCAMLPP_FLAGS)

# LOCAL_OCAMLCPP_FLAGS: Flags to pass to the macro-expander for [object].
#LOCAL_OCAMLCPP_FLAGS=$(GLOBAL_OCAMLCPP_FLAGS) $(MODULE_OCAMLCPP_FLAGS)

# LOCAL_LINK_FLAGS: Flags to pass to the linker for [object].
#LOCAL_LINK_FLAGS=$(GLOBAL_LINK_FLAGS) $(MODULE_LINK_FLAGS)

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


