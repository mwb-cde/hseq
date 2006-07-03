#####
# Makefile for building sub-directories
#
# Include Makefile.def to set variables to be used by Makefile.sub
# This should be copied into each directory to be built.
#####


-include Makefile.def

ifndef SRCCONFIGDIR
SRCCONFIGDIR = ../config
endif

# Don't try to build native code 
local_NATIVE := $(NATIVE)
NATIVE := "false"

# Also install hseqstart.ml

INITFILE ?= hseqstart.ml

INSTALL_OTHER_FILES += $(INITFILE)

include $(SRCCONFIGDIR)/Makefile.sub

NATIVE := $(local_NATIVE)
