# This is a silvercat file.

# First thing to do is to define the profile. It defines what main
# compilers should be used with what modification flags.
ag-profile gcc-native

# It's expected that "native" would be replaced with 32 or 64.


# The most low level command that defines targets to build.
# The first argument is always the target to be built.
# It is usually a symbolic name, not necessarily the name
# of the file.

# In case when this is a "program", on POSIX it is the same
# name of the resulting file. On Windows (Cygwin), however
# the program will get additionally .exe extention.

# The -category bin statement states that this program is
# to be installed as a binary application, on POSIX systems
# will be installed in $PREFIX/bin.

ag answer {
	-type 		program
	-category 	bin
	-sources 	file1.cc file2.cc
	-headers 	file.h
}

# For programs there can be later created a more convenient
# command:

# ag-programs { bin { answer -s {file1.cc file2.cc} -h file.h } }

