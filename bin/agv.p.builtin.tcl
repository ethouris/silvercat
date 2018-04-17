
# Executable file extensions per shared library extensions.
set exefor(.dll) .exe    ;# Windows
set exefor(.dylib) .app  ;# Mac
set exefor(.so) ""       ;# POSIX
set exefor(.library) ""  ;# Amiga :)

# Language recognition by file extension.
set langextmap {
	c {
		.c
	}

	c++ {
		.C .cc .cpp .cxx .c++
	}

	c-header {
		.h
	}

	c++-header {
		.H .hh .hpp .hxx .h++
	}

	objc {
		.m
	}

	objc++ {
		.M .mm .mpp .mxx .m++
	}
}

# Case of selecting common linker when a target uses
# sources in multiple languages.

# Don't know if this is the best way, but let it be...
# This map allows to select the best linker in case when
# you have a project with mixed languages. This below means,
# for example, that you can use C++ linker when you have
# sources in C++ and C languages, while you can use the
# C linker only if you have sources in C language.
set compatible_langs {
	c++ {
		c
	}
}

# Types of keys.
# The default key type is "list". The meaning is:

# list:
# updating the database adds the value at the end
#
# single:
# updating the database overwrites the current value
#
# command:
# The value is treated as a command-line. It's like single,
# but it's not treated as a single list element. Instead it's
# processed as a command-line - \n kept intact, and lines are
# merged, with taking care of that it's terminated with \n.
# 
# unique:
# updating the database adds the value at the end, like with
# list, with a restriction that each value must occur exactly
# once. An attempt to update default way with a non-unique
# value results in error. Doing "unique update" by {+- value}
# ignores the request if the value isn't unique.


set keytype(single) {
	compile
	compile_oflag
	link
	link_oflag
	linkdl
	gendep
	depspec
	preproc
	libtype
	name
	type
	headers-installdir
}

set keytype(command) {
	command
	clean
}

set keytype(unique) {
	depends
	packages
}

array set dbname {
	agv::target Target
	agv::info Info
	agv::profile Profile
}

array set typeflags {

	program {
		all
		{binddeps library}
	}

	library {
		all
		{binddeps library}
		{passdeps library}
	}
	custom all
	pkg-config all
}


