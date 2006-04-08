##
# OS generalisation
#
# Set variable OS to "mswin" for MS Windows settings (not tested)
##

##
# Unix settings (default)
##

# CHDIR: Change directory
CHDIR = cd

# MKDIR: Make a directory
MKDIR = mkdir 

# RMDIR: Remove a directory
RMDIR = rmdir

# COPY: Copy one or more files
COPY = cp -f

# RM: Delete one or more files
RM = rm -f

# SKIP: Do nothing
SKIP =

##
# OS independent toolbox Settings
##

ifdef TOOLBOX
ifneq ($(strip $(TOOLBOX)), "")

# Set the shell
#SHELL=cmd.exe

# CHDIR: Change directory
CHDIR = echo "CHDIR: not implemented"

# MKDIR: Make a directory
MKDIR = ocaml $(TOOLBOX) --mkdir

# RMDIR: Remove a directory
RMDIR = echo "RMDIR: not implemented"

# COPY: Copy one or more files
COPY = ocaml $(TOOLBOX) --cp

# RM: Delete one or more files
RM = ocaml $(TOOLBOX) --rm

# SKIP: Do nothing
SKIP = echo ""

endif
endif

##
# MS Windows Settings
##

#ifeq (${OS},"mswin")

# Set the shell
#SHELL=cmd.exe

# file: Convert a Unix file-name to the OS file-name
#file=$(subst /,\,$(1))

# CHDIR: Change directory
#CHDIR = cd

# MKDIR: Make a directory
#MKDIR = mkdir 

# RMDIR: Remove a directory
#RMDIR = rmdir /Q

# COPY: Copy one or more files
#COPY = xcopy /S /C /I /Y 

# RM: Delete one or more files
#RM = del /Q 

# SKIP: Do nothing
#SKIP = echo ""

#endif

