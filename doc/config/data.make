#
# Configuration dependent definitions 
# (relative to src directory)
#

ifdef SRCDIR
include $(SRCDIR)/../config/data.make
else
include ../../config/data.make
endif