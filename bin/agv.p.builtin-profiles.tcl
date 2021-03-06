
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
			std_option "Compiler option to use given language standard"
			std_values "key-value pairs to map general names of the language standard to value required to pass to that option"
		}

	}

	# { Options that apply by default unless overridden in particular profile }
	default {
		default {
			compile_oflag -o
			link_oflag -o
		}
	}

	posix-install {
		default {
			install:prefix /usr/local
			installdir:bin {$prefix/bin}
			# { XXX Mind that probably on 64-bit systems,
			# the 64-bit libraries are installed in lib64,
			# while lib is only for 32-bit libraries }
			installdir:lib {$prefix/lib}
			installdir:data {$prefix/share}
			installdir:include {$prefix/include}
		}
	}

	gcc-native {
		default {
			depspec auto
			depopt "-MMD -MF "
			defineflag -D
			libdirflag -L
			incdirflag -I
			std_option "-std="
			archive "ar rcs"
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
			depspec auto
			depopt "-MMD -MF "
			defineflag -D
			libdirflag -L
			incdirflag -I
			std_option "-std="
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

	# {XXX Currently you have to manually select profiles
	# in the Makefile.ag.tcl file. There should be added
	# a possibility to load a system-default C/C++ compiler
	# profile and in-system installation profile.}
}


