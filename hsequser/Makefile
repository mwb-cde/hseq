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
#####

#
# Building packed libraries (with option -pack) is complicated by the
# ocaml compiler requiring that all objects making up the packed
# library, together with their interfaces, must be in the same
# directory. The native-code compilers also require that a objects
# that is to be combined into a pack must be compiled with the
# -for-pack option.
#
# To support packing, the build system allows a 'packed' library L to
# be constructed from 'packing' objects O, with 'pack name' P. The
# packing objects are built in 'groups', with each group G built in a
# sub-directory, and marshalled into a packing directory named
# P_PACK_DIR. The packing groups for P are found in the subdirectories
# listed in P_GROUPS. The packed object P.cmo and its interface P.cmi
# are built and linked, in P_PACK_DIR, to construt packed library L.

#####
# Required definitions

PROJ_ROOT=..

# PACKED: List of packed libraries to build
#PACKED=hsequser

# SUBDIRS: List of subdirectories to build.
SUBDIRS=userlib

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
