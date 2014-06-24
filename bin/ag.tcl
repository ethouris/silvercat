#!/usr/bin/tclsh

# Get definitions of make.tcl as library
# XXX (find some better way to do it)
set was_interactive $tcl_interactive
set tcl_interactive 1
source [file dirname [info script]]/make.tcl
set tcl_interactive $was_interactive

namespace eval agv {
	set version 0.1 ;# just to define something

	variable target
	namespace export target

	# Supporting directories
	variable directories
	namespace export directories

	namespace eval p {

		set me [info script]

		set exefor(.dll) .exe
		set exefor(.dylib) .app
		set exefor(.so) ""

		proc lsuniq {ls} {
			set out ""
			foreach e $ls {
				if { $e ni $out } {
					lappend out $e
				}
			}

			return $out
		}

		proc PrepareGeneralTarget {} {
			# This procedure should collect all targets of
			# type 'program' or 'library' and create a phony
			# target named 'all' that has them all as dependencies

			set subtargets {}
			foreach t [array names agv::target] {
				set type [dict:at $agv::target($t) type]
				vlog "Target '$t', type $type:"
				if { $type in {program library} } {
					vlog " --> Added to 'all'"
					lappend subtargets $t
				} else {
					vlog " --| Not added to 'all'"
				}
			}

			ag all -type phony -depends {*}$subtargets
		}

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

			posix-install {
				default {
					prefix /usr/local
					installdir_bin {$prefix/bin}
					installdir_lib {$prefix/lib}
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

		namespace eval fw {
			proc pkg-config {target} {
				vlog "Running pkg-config framework for '$target'"
				# Take the package name, extract the library
				# parameters, apply to the database.
				set db $::agv::target($target)

				set packages [dict:at $db packages]
				vlog "Packages: $packages"
				foreach p $packages {
					if { [catch {exec pkg-config --exists $p}] } {
						error "Package not found: $p"
					}
					set ldflags [exec pkg-config --libs $p]
					set cflags [exec pkg-config --cflags $p]

					vlog "Data for $p: cflags='$cflags' ldflags='$ldflags'"

					dict lappend db ldflags {*}$ldflags
					dict lappend db cflags {*}$cflags
				}

				# Write back the database
				set ::agv::target($target) $db
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

	# Install prefix.
	# Will be set to the profile's default, unless overridden.
	variable prefix
	namespace export prefix
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

	foreach def [get agv::defines] {
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
	vlog "Extracting ingredients of '$source':"
	set gendep [dict:at $agv::profile($lang) gendep]

	set cmd "$gendep [ShellWrapAll $cflags] $source"
	vlog "Command: $cmd"

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
	set deps [lrange $deps 1 end]
	vlog "Resulting deps: $deps"

	return $deps
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

	if { $name in {default structure} } {
		error "Invalid profile name (keyword)"
	}

	if { ![dict exists $agv::p::profiles $name] } {
		error "No such profile: $name"
	}

	set prof ""
	if { ![array exists agv::profile] } {
		set prof [dict get $agv::p::profiles default]
	}
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
		fw { return frameworks }
	}

	return $alias
}


proc ag {target args} {
	set lastopt ""

	if { [llength $args] == 1 } {
		# May contain multiple lines, so wipe out comments
		set args [lindex [no_comment $args] 0]
	}

	# Get old options
	array set options [get agv::target($target)]

	foreach o $args {
		if { [string index $o 0] == "-" && [string index $o 1] != " " } {
			set lastopt [string range $o 1 end]
			set lastopt [UnaliasOption $lastopt]
			continue
		}

		# This time it's -option {- config speed}
		if { [string index $o 0] == "-" } {
			set o [lrange $o 1 end]
			set opt [get options($lastopt)]
			set pos ""

			foreach e $o {
				lappend pos {*}[lsearch -all -exact $opt $e]
			}

			set out ""
			set len [llength $opt]
			for {set i 0} {$i < $len} {incr i} {
				if { $i ni $pos } {
					lappend out [lindex $opt $i]
				}
			}
			set options($lastopt) $out
			continue
		}

		if { [string index $o 0] == "=" && [string index $o 1] == " " } {
			# Reset option (replace existing value)
			set options($lastopt) [string range $o 2 end]
			continue
		}

		lappend options($lastopt) $o
	}

	vlog "Target: $target {[array get options]}"

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
	set hsufs [concat [dict:at $agv::p::langextmap c-header] [dict:at $agv::p::langextmap c++-header]]

	vlog "Performing general processing for program '$target':"

	set hdrs ""
	foreach s $sources {
		set o [file rootname $s].ag.o

		# Now identify the programming language
		set lang [IdentifyLanguage $s]

		dict lappend used_langs $lang $s

		vlog " ... processing $s (language $lang) --> $o"
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

		# Find headers among dependent files in the rules
		set ifiles [lrange $rule 1 end-1]

		# Find header files in the deps.
		# If there are any, and they are not found in 'headers',
		# add them to noinst-headers
		foreach n $ifiles {
			if { [file extension $n] in $hsufs } {
				lappend hdrs $n
			}
		}

	}

	# Extract existing headers
	set th [dict:at $db headers]
	set tnh [dict:at $db noinst-headers]
	set hdrs [agv::p::lsuniq $hdrs]
	foreach h $hdrs {
		if { $h ni $tnh && $h ni $th } {
			lappend tnh $h
		}
	}
	dict set db noinst-headers $tnh

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

	set itarget install-$target
	set cat [dict:at $db category]
	set icmd ""
	set prefix [get agv::prefix]
	if { $prefix == "" } {
		set prefix [dict:at $agv::profile(default) prefix]
	}

	if { $prefix != "" } {
		switch -- $cat {
			noinst {
				# Nothing.
			}

			bin {
				set bindir [subst -nocommands [dict:at $agv::profile(default) installdir_bin]]
				if { $bindir != "" } {
					set icmd "
	install $outfile $bindir
"
				}
			}
		}
	}

	if { $icmd != "" } {
		dict set rules $itarget [list $outfile $icmd]
		# Make the install target phony
		dict set phony $itarget ""
	}

	# Ok, ready. Write back to the database
	dict set db sources $sources
	dict set db objects $objects
	dict set db rules $rules
	dict set db phony $phony

	set agv::target($target) $db

	Process:phony $target
}

proc Process:phony target {
	# Do the general depends processing
	set phony [dict:at $agv::target($target) phony]
	set deps [dict:at $agv::target($target) depends]

	# Intentionally 2 elements as it should be a dict
	lappend phony [list $target $deps]

	dict set agv::target($target) phony $phony
}

proc GenerateCompileRule {db lang objfile source} {

	# General cflags for the target
	set cflags [dict:at $db cflags]

	# General language cflags applicable for all targets
	# For example, definitions will be collected here for -D option
	# The value of agv::cflags($lang) will be also included
	set lang_cflags [GenerateCompileFlags $lang]

	vlog "Generating compile rule for '$objfile':"
	vlog " ... target's cflags: $cflags"
	vlog " ... lang($lang) cflags: $lang_cflags"

	append cflags " $lang_cflags"

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
	vlog "Generating link rule for '$outfile' ldflags: $ldflags"

	set linker [dict get $agv::profile($lang) link]
	set oflag [dict get $agv::profile($lang) link_oflag]

	set command "$linker $objects $oflag $outfile $ldflags"

	set rule "$objects {\n\t$command\n}"

	return $rule
}

proc ag-genrules target {

	# Complete lacking values that have to be generated.
	if { ![agp-prepare-database $target] } {
		puts stderr "Failed to prepare database for '$target'"
		return false
	}

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

	# Clean rule
	puts "rule $target-clean {
	!tcl autoclean $target
}"
}

proc ag-make {target} {

	if { [llength $target] > 1 } {
		set exp_targets [lrange $target 1 end]
		set target [lindex $target 0]
	} else {
		set exp_targets $target
	}

	vlog "Preparing database for '$target' to make '$exp_targets'"
	if { ![agp-prepare-database $target] } {
		puts "Failed to prepare database for '$target'"
		return
	}

	set rules [dict:at $agv::target($target) rules]

	# Now define the rules according to the 
    set phony [dict:at $agv::target($target) phony]

	if { $phony != "" } {
		foreach {rule deps} $phony {
			vlog "Preparing phony: $rule $deps"
			phony $rule {*}$deps
		}
	}

	set rules [dict:at $agv::target($target) rules]

	# Rules is itself also a dictionary.
	# Key is target file, value is dependencies and command at the end.

	foreach {tarfile data} $rules {
		vlog "Preparing rule: $tarfile [lrange $data 0 end-1]"
		rule $tarfile {*}$data
	}

	rule $target-clean "
		!tcl autoclean $target
	"

	make {*}$exp_targets
}

proc agp-prepare-database target {
	vlog "--- Preparing database for target '$target'"

	# Auto-generate target "all", if not defined
	if { $target == "all" && ![info exists agv::target(all)] } {
		vlog "--- Synthesizing 'all' target"
		agv::p::PrepareGeneralTarget
	}

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
	vlog "Preparing database for '$target' type=$type"

	set frameworks [dict:at $agv::target($target) frameworks]

	# XXX default frameworks
	if { $frameworks == "" } {
		set frameworks pkg-config
	}
	vlog " ... Frameworks: $frameworks"
	foreach frm $frameworks {
		if { [string first : $frm] == -1 } {
			# Treat it as a builtin, that is agv::p::fw::$NAME
			set frm "::agv::p::fw::$frm"

			if { [info command $frm] != $frm } {
				error "No such BUILTIN framework '$frm' (external fw must use namespaces)"
			}
		}

		if { [catch {$frm $target} err] } {
			puts stderr "Error executing framework '$frm' on '$target':\n$err"
			return false
		}
	}

	Process:$type $target
	
	vlog "DATABASE AFTER PROCESSING:"
	foreach k [dict keys $agv::target($target)] {
		vlog "  -$k: [dict get $agv::target($target) $k]"
	}

	vlog "PROCESSING DEPENDS:"

	foreach dep [dict:at $agv::target($target) depends] {
		if { ![agp-prepare-database $dep] } {
			return false
		}
	}

	return true
}

proc ag-subdir args {
	if { [llength $args] == 1 } {
		set args [lindex $args 0]
	}

	lappend agv::p::directories {*}$args
}

proc ag-help {args} {
	puts "Usage: [file tail $::argv0] genrules <target>"
}

if { !$tcl_interactive } {

if { ![file exists Makefile.ag.tcl] } {
	puts stderr "File not found: Makefile.ag.tcl"
	exit 1
}

source Makefile.ag.tcl

set arg1 [lindex $argv 0]
if { $arg1 == "" } {
	set arg1 help
}
ag-$arg1 [lrange $argv 1 end]


}
