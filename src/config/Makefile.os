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

##
# MS Windows Settings
##

#ifeq ($(OS),"mswin")

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

#endif

