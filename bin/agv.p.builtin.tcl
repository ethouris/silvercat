
set exefor(.dll) .exe
set exefor(.dylib) .app
set exefor(.so) ""

# Utilities
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
}

set keytype(command) {
	command
	clean
}

set keytype(unique) {
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


