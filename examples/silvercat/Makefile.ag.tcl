# This is a silvercat file.

# Make things loud (this file is used for testing)
set ::g_verbose 1

# Usual minimum version requirement - using standard Tcl mechanism.
package require ag 0.8

# First thing to do is to define the profile. It defines what main
# compilers should be used with what modification flags.

ag-profile gcc-native      ;# Profile for compiler
ag-profile posix-install   ;# Profile for installation rules

ag-profile general -fw test:ag pkg-config

namespace eval test:ag {
	proc prepare {target} {
		set dbv agv::target($target)
		puts "HA!!!!!! PREPARE test for [pget $dbv.type] '$target'!"
	}
}


# It's expected that "native" would be replaced with 32 or 64.

# The most low level command that defines targets to build.

# ag answer {
# 	-type 		program
# 	-category 	bin
# 	-sources 	file1.cc file2.cc
# 	-headers 	file.h
# 	-packages   zlib
# }

ag answer -type program -category bin ;#-fw pkg-config (this is default if no framework is explicitly set)
# (NOTE: if you are using any frameworks and would like to use pkg-config among others,
# you have to declare it explicitly - although framework hooks are public functions, so
# dependent frameworks may use them internally).
ag answer -packages zlib-1.2
ag answer -sources file1.cc file2.cc

ag ff -type library -category lib -sources file2.cc -headers file.h
# Ups, file2 should be removed from the answer file!
ag answer -sources {- file2.cc}
# This demonstrated how to add values that have "-" as the first character
# This option is here blocked because it's for the libff.a library, same as
# adding the ff target as dependent.
#ag answer -ldflags -- -L.  -lff
ag answer -depends ff

# Define explicitly includes in this file. When this is not defined,
# Silvercat will try to autodetect includes by running gendep-mapped command.
#ag-info file1.cc -includes file.h
#ag-info file2.cc -includes ""


# Headers will still be extracted to -noinst-headers if detected by deps checker
# (or declared explicitly in the fileinfo database for that file),
# however they will be added to -noinst-header!
# Although this is meaningless because header files are not to be installed for
# the program-type target. That only happens for library-type targets.
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

