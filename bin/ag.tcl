#!/usr/bin/tclsh

namespace eval agv {
	set version 0.1 ;# just to define something

	variable target
	namespace export target

	namespace eval p {
		set exefor(.dll) .exe
		set exefor(.dylib) .app
		set exefor(.so) ""


		proc Get variable {
			upvar $variable v
			if { ![info exists v] } {
				return
			}
			return $v
		}

		set langextmap {
			c {
				.c
			}

			c++ {
				.C .cc .cpp .cxx .c++
			}

			objc {
				.m
			}

			objc++ {
				.M .mm .mpp .m++
			}
		}

		set profiles {

			structure {

				# { Lang entry named "general" defines things that are common
				# for all languages. }
				lang {
					compile "Command to produce .o file from source file"
					compile_oflag "Usually -o"
					link "Command to produce executable file"
					link_oflag "Usually -o"
					linkdl "(optional) Command to produce dynamic library file (defaults to link)"
					gendep "Command to generate dependencies (produces a list of all files that can be extracted of a single source file)"
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
				}

				c {
					compile "clang -c"
					link "clang"
				}
			}
		}

		# Don't know if this is the best way, but let it be...
		set compatible_langs {
			c++ {
				c d
			}

			c {
			}
		}

	}

	set exe $p::exefor([info sharedlibextension])
	namespace export exe

	# Profile is an array indexed by language name.
	variable profile
	namespace export profile

	# Definitions (define flags)
	# They can be as just one thing or variable=value
	variable defines
	namespace export defines
}

proc GetCommonLanguage langs {

	if { [llength $langs] < 2 } {
		# No dillema :)
		return $langs
	}

	set accepted ""

	foreach a $langs {
		foreach b $langs {
			if { $a == $b } {
				continue
			}

			#puts "--> Check if $a can supersede $b"

			set c [dict:at $agv::p::compatible_langs $a]
			#puts "---> Compatible languages for $a: $c"

			if { $b in $c } {
				#puts "-- $b is one."
				lappend accepted $a
			} else {
				#puts "-- $b is not one"
			}
		}
	}

	return [lindex $accepted 0]
}

proc GenerateCompileFlags lang {
	set define_flags ""
	foreach def [dict:at $agv::profile($lang) defines] {
		lappend define_flags -D$def
	}

	foreach def [agv::p::Get agv::defines] {
		lappend define_flags -D$def
	}

	set cflags [dict:at $agv::profile($lang) cflags]

	return "$define_flags $cflags"
}

proc ShellWrap arg {
	if { [string first " " $arg] != -1 } {
		return "\"$arg\""
	}

	if { [string index $arg 0] == "\{" } {
		return "\"[string range $arg 1 end-1]\""
	}

	return $arg
}

proc ShellWrapAll arg {
	set out ""
	foreach a $arg {
		append out "[ShellWrap $arg] "
	}
}

proc GenerateDepends {lang cflags source} {
	set gendep [dict:at $agv::profile($lang) gendep]

	set cmd "$gendep [ShellWrapAll $cflags] $source"

	# Run the command to generate deps
	#puts "Command: "
	#foreach c $cmd {
#		puts " --> $c"
#	}

	set deps [exec {*}$cmd]

	# Rules are generated in the convention of "make".
	# Make them a plain list, as needed for "make.tcl"
	set deps [string map { "\\\n" "" } $deps]

	# Drop the *.o target, we don't need it.
	return [lrange $deps 1 end]
}

proc IdentifyLanguage sourcefile {

	set fext [file extension $sourcefile]

	foreach {lang ext} $agv::p::langextmap {
		if { $fext in $ext } {
			return $lang
		}
	}
}

proc DetectType target {
	# If any dot is found, try to determine by extension.
	# If no dot found, return "program".

	if { [string first $target .] == -1 } {
		return program
	}

	set ext [file extension $target]

	if { $ext == [info sharedlibextension] } {
		return library
	}

	if { $ext in {.lib .a} } {
		return archive
	}
}

proc ag-profile name {

	if { $name in {general structure} } {
		error "Invalid profile name (keyword)"
	}

	if { ![dict exists $agv::p::profiles $name] } {
		error "No such profile: $name"
	}

	set prof [dict get $agv::p::profiles default]
	set prof [dict merge $prof [dict get $agv::p::profiles $name]]

	# Now merge every language item with default.
	# Leave the default untouched, however.

	set deflt [dict:at $prof default]
	foreach lng [dict keys $prof] {
		if { $lng == "default" } {
			continue
		}

		dict set prof $lng [dict merge [dict:at $prof default] [dict:at $prof $lng]]
	}

	array set agv::profile $prof
}

proc UnaliasOption alias {
	switch -- $alias {
		s { return sources }
		h { return headers }
		nh { return noinst-headers }
	}

	return $alias
}


proc ag {target args} {
	set lastopt ""

	foreach o $args {
		if { [string index $o 0] == "-" } {
			set lastopt [string range $o 1 end]
			continue
		}

		lappend options($lastopt) [UnaliasOption $o]
	}

	set agv::target($target) [array get options]
}


proc dict:at {dic args} {
	if { ![dict exists $dic {*}$args] } {
		return ""
	}

	return [dict get $dic {*}$args]
}

proc Process:program target {
	# If the target is program, then you need
	# to generate rules that compile all sources
	# into *.o files, then link them together
	# using either global libraries or packages.
	set db $agv::target($target)

	set sources [dict:at $db sources]
	set objects [dict:at $db objects]
	set rules [dict:at $db rules]

	set used_langs ""

	foreach s $sources {

		set o [file rootname $s].ag.o

		# Now identify the programming language
		set lang [IdentifyLanguage $s]

		dict lappend used_langs $lang $s

		# Generate rule for the target
		set rule [GenerateCompileRule $db $lang $o $s]

		# Add rule to objects, notify in the database
		if { [lsearch $objects $o] == -1 } {
			lappend objects $o
		}

		# Don't override rules that are set already.
		if { ![dict exists $rules $o] } {
			dict set rules $o $rule
		}
	}

	set lang [GetCommonLanguage [dict keys $used_langs]]

	# ok, we have all sources processed.
	# Now we need to generate the rule for
	# main program.
	set outfile $target$agv::exe

	# Collect libraries.
	# Turning packages from pkg-config or whatever
	# other systems should be done before calling this
	# function.

	set rule [GenerateLinkRule:program $db $lang $outfile $objects [dict:at $db ldflags]]
	dict set rules $outfile $rule

	# Add a phony rule that redirects the symbolic name to physical file,
	# in case they differ (in future, this can be due to them being in different directories)

	set phony ""
	if { $outfile != $target } {
		dict set phony $target $outfile
	}

	# Ok, ready. Write back to the database
	dict set db sources $sources
	dict set db objects $objects
	dict set db rules $rules
	dict set db phony $phony

	set agv::target($target) $db
}

proc GenerateCompileRule {db lang objfile source} {

	# General cflags for the target
	set cflags [dict:at $db cflags]

	# General language cflags applicable for all targets
	# For example, definitions will be collected here for -D option
	# The value of agv::cflags($lang) will be also included
	append cflags " [GenerateCompileFlags $lang]"

	# The rule should be formed as for make.tcl
	# except the 'rule' command and the target name.
	# So: source-deps... command-to-compile

	set deps [GenerateDepends $lang $cflags $source]

	set compiler [dict get $agv::profile($lang) compile]
	set oflag [dict get $agv::profile($lang) compile_oflag]

	set command "$compiler $cflags $source $oflag $objfile"

	set rule "$deps {\n\t$command\n}"
}

proc GenerateLinkRule:program {db lang outfile objects ldflags} {

	set linker [dict get $agv::profile($lang) link]
	set oflag [dict get $agv::profile($lang) link_oflag]

	set command "$linker $objects $oflag $outfile $ldflags"

	set rule "$objects {\n\t$command\n}"

	return $rule
}

proc ag-genrules target {
	# Check if defined

	if { ![info exists agv::target($target)] } {
		error "No such target: $target"
	}

	set type [dict:at $agv::target($target) type]

	if { $type == "" } {
		set type [DetectType $target]
		if { $type == "" } {
			error "Can't recognize target type for '$target' - please declare explicitly"
		}
	}

	Process:$type $target
    set phony [dict:at $agv::target($target) phony]

	if { $phony != "" } {
		foreach {rule deps} $phony {
			puts "phony $rule $deps"
		}
	}

	set rules [dict:at $agv::target($target) rules]

	# Rules is itself also a dictionary.
	# Key is target file, value is dependencies and command at the end.

	foreach {tarfile data} $rules {
		puts "rule $tarfile $data"
	}
}

if { !$tcl_interactive } {

# TEST
cd ../examples/tclmake

ag-profile gcc-native

# Makefile.ag.tcl contents
ag answer -type program -sources file1.cc file2.cc -headers file.h


ag-genrules answer

}
