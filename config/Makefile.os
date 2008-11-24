##
# OS generalisation
#
# Set variable OS to "mswin" for MS Windows settings (not tested)
##

##
# Unix settings (default)
##

# CD: Change directory
export CD = cd

# MKDIR: Make a directory
export MKDIR = mkdir -p

# RMDIR: Remove a directory
export RMDIR = rmdir

# COPY: Copy one or more files
export COPY = cp -f

# RM: Delete one or more files
export RM = rm -f

# SKIP: Do nothing
export SKIP =

##
# OS independent toolbox Settings
##

ifdef TOOLBOX
ifneq ($(strip $(TOOLBOX)), "")

# Set the shell
#export SHELL=

# CD: Change directory
export CD = echo "CD: not implemented"

# MKDIR: Make a directory
export MKDIR = ocaml $(TOOLBOX) --mkdir

# RMDIR: Remove a directory
export RMDIR = echo "RMDIR: not implemented"

# COPY: Copy one or more files
export COPY = ocaml $(TOOLBOX) --cp

# RM: Delete one or more files
export RM = ocaml $(TOOLBOX) --rm

# SKIP: Do nothing
export SKIP = echo ""

endif
endif

##
# MS Windows Settings
##

#ifeq (${OS},"mswin")

# Set the shell
#export SHELL=cmd.exe

# file: Convert a Unix file-name to the OS file-name
#export file=$(subst /,\,$(1))

# CD: Change directory
#export CD = cd

# MKDIR: Make a directory
#export MKDIR = mkdir 

# RMDIR: Remove a directory
#export RMDIR = rmdir /Q

# COPY: Copy one or more files
#export COPY = xcopy /S /C /I /Y 

# RM: Delete one or more files
#export RM = del /Q 

# SKIP: Do nothing
#export SKIP = echo ""

#endif

