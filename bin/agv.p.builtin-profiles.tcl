

# REMEMBER!!!
# In here below ALL COMMENTS MUST BE # { ... }
# This is because it's only a dataset and it's interpreted
# specifically; comments are removed just by removing the '#' key.

set profiles {

	# {This is only informative - explains the structure of the profiles.}
	structure {

		# { Lang entry named "default" defines things that are common
		# for all languages. }
		lang {
			compile "Command to produce .o file from source file"
			compile_oflag "Usually -o"
			link "Command to produce executable file"
			link_oflag "Usually -o"
			link_lflag "Usually -l"
			linkdl "(optional) Command to produce dynamic library file (defaults to link with dlflag)"
			dlflag "Flag that should be added when compiling a dynamic library, using 'link'"
			archive "Command to create static libraries (archives)"

			# { The format of the output data for gendep should be as it's generated for Makefile, that is:
			#	 - The first word in the line is the target name followed by a colon
			#    - Then space-separated filenames of ingredients in one line
			#    - Lines may be extended by backslash
			# If you want to make a custom dependency generator, just note that the first word is dropped
			# (output file is already known), so it need not be ended by a colon and it needn't be anything
			# meaningful at all. Just must be present and consist of non-white characters.
			# }
			gendep "Command to generate dependencies
					(produces a list of all files that can be extracted of a single source file)"
			depspec "One of: auto, cached, explicit:
			         auto: dependencies are generated at generation time and stored in Makefile.tcl
					 cached: dependencies are generated at build time and stored in *.d files
					 explicit: dependencies must be taken care of manually, nothing is automatically done"
			preproc "Command to run preprocessor (optional, can be empty if a language doesn't use one)"
			cflags "Flags passed always to compile command (compile, gendep, preproc)"
			ldflags "Flags passed to link command"
			ldstatic "Option to link statically"
			std_option "Compiler option to use given language standard"
			std_values "key-value pairs to map general names of the language standard to value required to pass to that option"

			# { Install-related keys }
			install:prefix "Main directory with installation structure"
			installdir:bin "Installation directory for 'bin' category"
			installdir:lib "Installation directory for 'lib' category"
			installdir:lib:shared "Installation directory for 'lib' category for shared libraries"
			installdir:data "Installation directory for 'data' category"
			installdir:include "Installation directory for header files (install-target-headers)"

			cmd:makedir "command to make directory (full tree)"
			cmd:install "command to put a file into an installation directory"

			form:archive "name format for static libraries"
			form:sharedroot "name format for shared libraries, without extension (this will be taken from \[info sharedlibextension])"
		}

	}

	# { Options that apply by default unless overridden in particular profile }
	default {
		default {
			compile_oflag -o
			link_oflag -o
			link_lflag -l

			form:archive "lib%.a"
			form:sharedroot "lib%"
		}
	}

	posix-install {
		default {
			install:prefix /usr/local
			installdir:bin {$prefix/bin}
			installdir:lib {$prefix/lib$libsuffix}

			installdir:lib:shared {[expr {[info sharedlibextension] == ".dll" ? "$prefix/bin" : "$prefix/lib"}]}

			installdir:data {$prefix/share}
			installdir:include {$prefix/include}

			cmd:makedir "mkdir -p"
			cmd:install "cp -a"
		}
	}

	gcc-native {
		default {
			version "gcc -v"
			targetspec "Target: "
			depspec auto
			depopt "-MMD -MF "
			defineflag -D
			libdirflag -L
			incdirflag -I
			std_option "-std="
			archive "ar rcs"
			ldstatic "-static"
		}
		c++ {
			compile "g++ -c"
			link "g++"
			linkdl "g++ -shared"
			gendep "g++ -MM"
			std_values {
				c++ c++03
				c++98 c++03
				c++11 c++0x
				c++14 c++1y
				c++17 c++1z
				gnu-c++98 gnu++98
				gnu-c++03 gnu++03
				gnu-c++0x gnu++0x
				gnu-c++11 gnu++0x
				gnu-c++1y gnu++1y
				gnu-c++14 gnu++1y
				gnu-c++1z gnu++1z
				gnu-c++17 gnu++1z
				"" ""
			}
		}

		c {
			compile "gcc -c"
			link "gcc"
			linkdl "gcc -shared"
			gendep "gcc -MM"
			std_values {
				c c90
				"" ""
			}
		}
	}

	clang-native {
		default {
			version "clang -v"
			targetspec "Target: "
			depspec auto
			depopt "-MMD -MF "
			defineflag -D
			libdirflag -L
			incdirflag -I
			std_option "-std="
			archive "ar rcs"
			ldstatic "-static"
		}
		c++ {
			compile "clang++ -c"
			link "clang++"
			linkdl "clang++ -shared"
			gendep "clang++ -MM"
			depspec auto
			std_values {
				c++ c++03
				c++98 c++03
				c++11 c++0x
				c++14 c++1y
				c++17 c++1z
				gnu-c++98 gnu++98
				gnu-c++03 gnu++03
				gnu-c++11 gnu++0x
				gnu-c++14 gnu++1y
				gnu-c++17 gnu++1z
				"" ""
			}
		}

		c {
			compile "clang -c"
			link "clang"
			linkdl "clang -shared"
			gendep "clang -MM"
			depspec auto
			std_values {
				c c90
				"" ""
			}
		}
	}

	%gcc-template {
		default {
			version "$cc -v"
			targetspec "Target: "
			depspec auto
			depopt "-MMD -MF "
			defineflag -D
			libdirflag -L
			incdirflag -I
			std_option "-std="
			archive "ar rcs"
			ldstatic "-static"
		}
		c++ {
			compile "$cxx $cxxflags $cppflags -c"
			link "$cxx $ldflags"
			linkdl "$cxx $ldflags -shared"
			gendep "$cxx $cxxflags $cppflags -MM"
			std_values {
				c++ c++03
				c++98 c++03
				c++11 c++0x
				c++14 c++1y
				c++17 c++1z
				gnu-c++98 gnu++98
				gnu-c++03 gnu++03
				gnu-c++0x gnu++0x
				gnu-c++11 gnu++0x
				gnu-c++1y gnu++1y
				gnu-c++14 gnu++1y
				gnu-c++1z gnu++1z
				gnu-c++17 gnu++1z
				"" ""
			}
		}

		c {
			compile "$cc $cflags $cppflags -c"
			link "$cc $ldflags"
			linkdl "$cc $ldflags -shared"
			gendep "$cc $cflags $cppflags -MM"
			std_values {
				c c90
				"" ""
			}
		}
	}

	# {XXX Currently you have to manually select profiles
	# in the Makefile.ag.tcl file. There should be added
	# a possibility to load a system-default C/C++ compiler
	# profile and in-system installation profile.}
}

proc import-env {name deflt} {
	set varname [string tolower $name]
	upvar $varname vv
	set vv [::pget ::env($name) $deflt]
}

# Using a procedure to avoid adding variables
proc create_cc_custom {profiles} {
	# Profiles created with the use of environment variables
	set cc_custom_tpl [dict get $profiles %gcc-template]

	import-env CC cc
	import-env CXX c++
	import-env CFLAGS ""
	import-env CXXFLAGS ""
	import-env CPPFLAGS ""
	import-env LDFLAGS ""

	set cc_custom_resolv [subst $cc_custom_tpl]

	return $cc_custom_resolv
}

dict set profiles cc-custom [create_cc_custom $profiles]


