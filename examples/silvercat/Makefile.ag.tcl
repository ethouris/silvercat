# This is a silvercat file.

# Make things loud (this file is used for testing)
#set mkv::p::verbose 1

# Usual minimum version requirement - using standard Tcl mechanism.
package require ag 0.8

# First thing to do is to define the profile. It defines what main
# compilers should be used with what modification flags.

# NOTE: this is temporary. In future Silvercat will have system
# autodetection and it will apply the default profile automatically.
# It's expected that "native" would be replaced with 32 or 64.
ag-profile gcc-native      ;# Profile for compiler
ag-profile posix-install   ;# Profile for installation rules

# The pkg-config framework is declared by default, unless you set up
# any frameworks by yourself - in this case you have to add it explicitly
# (The reason is that pkg-config may interfere with other frameworks or be
# needed by other frameworks and the order of declaration may matter).

#ag-profile general -fw test:ag pkg-config

# Well, our own framework. Just for testing to see if it works.
# The framework name must be a namespace name and it must contain
# at least one colon in the referring name. That is, either it must
# contain a semicolon in the name, or it can be a second-nested
# namespace.
# --- namespace eval test:ag {
# --- 	proc prepare {target} {
# --- 		set dbv agv::target($target)
# --- 		puts "HA!!!!!! PREPARE test for [pget $dbv.type] '$target'!"
# --- 	}
# --- }

# The most low level command that defines targets to build.

# ag answer {
# 	-type 		program
# 	-install 	bin
# 	-sources 	file1.cc file2.cc
# 	-headers 	file.h
# 	-packages   zlib
# }

ag-subdir party

ag-profile c++ -defines ANSWER

ag answer -type program -install bin ;#-fw pkg-config (framework can be also set per target)
ag answer -packages zlib-1.2
ag answer -sources file1.cc file2.cc
ag answer -depends party/test

ag answer -defines DEBUG -incdir $env(HOME)/.local/include -libdir $env(HOME)/.local/lib

ag ff -type library -install lib -sources file2.cc -headers file.h
# Ups, file2 should be removed from the answer file!
ag answer -sources {- file2.cc}
# This demonstrated how to add values that have "-" as the first character
# This option is here blocked because it's for the libff.a library, same as
# adding the ff target as dependent.
#ag answer -ldflags -- -L.  -lff

# Setting a dependency on a target that was previously defined as of type library,
# and when the target is a program, this statement makes the program to be linked
# against the library built for that target. If you want to make the target simply
# dependent on a library target, but not to be linked against it, make an intermediate
# phony target and link them with dependencies. The phony target will not ship libraries,
# so linkage against the library won't happen, and phony target being dependent on
# a library only does forwarding when compiling.
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

