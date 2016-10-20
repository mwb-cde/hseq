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
# SUBDIRS: List of sub-directories to recurse into.
# LIBRARIES: List of libraries to build.
# PROGRAMS: List of programs to build.
# MODULE_INST_PATH: PATH to append to the relative installation path.
# MODULE_USE_OBJDIR [yes/no]: Whether to use the staging directory.
# MODULE_OBJ_PATH: PATH to append to the staging directory path.
#
# For each program or library [object] listed in LIBRARIES or PROGRAMS:
#
# object_SOURCES: List of source file names, without suffixes.
# object_INCLUDE: Directories to include when compiling [object].
# object_LIBS: Libraries to include when linking [object].
# object_OCAMLC_FLAGS: Flags to pass to the compiler for [object].
# object_OCAMLPP_FLAGS: Flags to pass to the preprocessor for [object].
# object_LINK_FLAGS: Flags to pass to the linker for [object].
# object_INST_PATH: PATH to append to the relative installation path.
#
#
# Notes:
#
# All object-specific flags are appended to the flags passed to
# the relevant tool.
#
#
# An object will be installed into the directory formed by
#       $(INST_PATH)/$(MODULE_INST_PATH)/$(OBJECT_INST_PATH)
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
#SUBDIRS=

# LIBRARIES: List of libraries to build.
LIBRARIES=tpquote

# MODULE_USE_OBJDIR [yes/no]: Whether to use the staging directory.
MODULE_USE_OBJDIR=yes

# Prepropcessor options.
MODULE_OCAMLPP_FLAGS=-pp "$(CAMLPO) q_MLast.cmo" -I $(CAMLPLIB)

# DISABLE_NATIVECODE: If yes, don't generate native-code
DISABLE_NATIVECODE=yes

#####
# Object building settings
#

# object_SOURCES: List of source file names, without suffixes.
#tpquote_SOURCES=tpquote
tpquote_SOURCES=tpquote

# object_INCLUDE: Directories to include when compiling [object].
#object_INCLUDE=
tpquote_INCLUDE=

# object_LIBS: Libraries to include when linking [object].
#object_LIBS=

# object_OCAMLC_FLAGS: Flags to pass to the compiler for [object].
#object_OCAMLC_FLAGS=
tpquote_OCAMLC_FLAGS=

# object_OCAMLPP_FLAGS: Flags to pass to the preprocessor for [object].
#object_OCAMLPP_FLAGS=

# object_LINK_FLAGS: Flags to pass to the linker for [object].
#object_LINK_FLAGS=

# object_INST_PATH: PATH to append to the relative installation path.
#object_INST_PATH=

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
