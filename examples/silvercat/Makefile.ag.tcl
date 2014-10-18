# This is a silvercat file.

set ::g_verbose 1

# First thing to do is to define the profile. It defines what main
# compilers should be used with what modification flags.


ag-profile gcc-native      ;# Profile for compiler
ag-profile posix-install   ;# Profile for installation rules

# It's expected that "native" would be replaced with 32 or 64.

# The most low level command that defines targets to build.

# ag answer {
# 	-type 		program
# 	-category 	bin
# 	-sources 	file1.cc file2.cc
# 	-headers 	file.h
# 	-packages   zlib
# }

ag answer -type program -category bin
ag answer -packages zlib
ag answer -sources file1.cc file2.cc

ag ff -type library -category lib -sources file2.cc -headers file.h

# Define explicitly includes in this file. When this is not defined,
# Silvercat will try to autodetect includes by running gendep-mapped command.
#ag-info file1.cc -includes file.h


# (headers will still be extracted to -noinst-headers if detected by deps checker,
# however they will be added to -noinst-header!)
#ag answer -headers file.h


# The first argument is always the target to be built.
# It is usually a symbolic name, not necessarily the name
# of the file.

# In case when this is a "program", on POSIX it is the same
# name of the resulting file. On Windows (Cygwin), however
# the program will get additionally .exe extention.

# The -category bin statement states that this program is
# to be installed as a binary application, on POSIX systems
# will be installed in $PREFIX/bin.

# For programs there can be later created a more convenient
# command:

# ag-programs { bin { answer -s {file1.cc file2.cc} -h file.h } }

