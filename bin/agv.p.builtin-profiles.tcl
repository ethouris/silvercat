
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
			linkdl "(optional) Command to produce dynamic library file (defaults to link)"

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
			preproc "Command to run preprocessor (optional, can be empty if a language doesn't use one)"
			cflags "Flags passed always to compile command (compile, gendep, preproc)"
			ldflags "Flags passed to link command"
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
			prefix /usr/local
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
		c++ {
			compile "g++ -c"
			link "g++"
			linkdl "g++ -dynamic"
			gendep "g++ -MM"
		}

		c {
			compile "gcc -c"
			link "gcc"
			linkdl "gcc -dynamic"
			gendep "gcc -MM"
		}
	}

	clang-native {
		c++ {
			compile "clang++ -c"
			link "clang++"
			linkdl "clang++ -dynamic"
			gendep "clang++ -MM"
		}

		c {
			compile "clang -c"
			link "clang"
			linkdl "clang -dynamic"
			gendep "clang -MM"
		}
	}

	# {XXX Currently you have to manually select profiles
	# in the Makefile.ag.tcl file. There should be added
	# a possibility to load a system-default C/C++ compiler
	# profile and in-system installation profile.}
}


