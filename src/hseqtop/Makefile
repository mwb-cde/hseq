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

include $(SRCCONFIGDIR)/Makefile.sub

NATIVE := $(local_NATIVE)
