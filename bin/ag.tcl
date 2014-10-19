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

		# Prologue
		set me [info script]
		set here [file dirname $me]

		# Ingredients

		source $here/agv.p.utilities.tcl
		source $here/agv.p.builtin.tcl
		source $here/agv.p.builtin-profiles.tcl
		source $here/agv.p.builtin-frameworks.tcl

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

	# Per-file set information
	variable fileinfo
	namespace export fileinfo
}

namespace import agv::p::dict:at

proc GenerateCompileFlags lang {
	set define_flags ""
	foreach def [dict:at $agv::profile($lang) defines] {
		lappend define_flags -D$def
	}

	foreach def [pget agv::defines] {
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
	set deps [string map { "\\\n" " " } $deps]
	# Drop the *.o target, we don't need it.
	set deps [lrange $deps 1 end]
	vlog "Resulting deps: $deps"

	return $deps
}

proc ExecuteFrameworks {target step args} {
	set frameworks [dict:at $agv::target($target) frameworks]
	vlog " ... Frameworks/$step: $frameworks ($args)"
	foreach frm $frameworks {
		if { [string first : $frm] == -1 } {
			# Treat it as a builtin, that is agv::p::fw::$NAME
			set frm "::agv::p::fw::$frm"

			if { ![namespace exists $frm] } {
				error "No such BUILTIN framework '$frm' (external fw must use namespaces)"
			}
		}

		if { [string index $frm 0] != ":" } {
			set frm "::$frm"
		}

		set procname [join [list $frm :: $step] ""]
		if { [info command $procname] != $procname } {
			continue
		}

		vlog "... Found step $step for framework '$frm'"

		if { [catch {$procname $target {*}$args} err] } {
			puts stderr "Error executing framework '$procname' on '$target':\n$err"
			return false
		}
	}
	return true
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

	# XXX This is true only on POSIX.
	# Mind the $agv::exe value as a suffix.
	# On Windows and Mac the no-dot files should
	# be reported as Unknown.

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


proc AccessDatabase {array target args} {

	upvar $array agv_db

	set lastopt ""

	if { [llength $args] == 1 } {
		# May contain multiple lines, so wipe out comments
		set args [lindex [no_comment $args] 0]
	}

	if { ![info exists agv_db($target)] } {
		# Make the database created, if not yet exists
		dict set agv_db($target) name $target
	}

	# Get old options
	array set options $agv_db($target)

	set query ""
	set nomoreoptions false

	foreach o $args {
		if { $query != "" } {
			# This is considered argument for the query, which is a mask.
			set current [dict:at $agv_db($target) $query]
			set out ""
			foreach v $current {
				if { [string match $o $v] } {
					lappend out $v
				}
			}
			# This query is not to modify the database - return immediately
			return $out
		}

		if { [string index $o 0] == "?" } {
			if { [string length $o] < 2 } {
				error "Error in specification - query: The '?' character must be followed by the key name"
			}
			set query [string range $o 1 end]
			continue
		}

		if { $o == "--" } {
			set nomoreoptions true
			continue
		}

		set f2 [string range $o 0 1]

		# This time it's -option {- config speed} - means remove these items
		if { $f2 == "- " } {
			set o [lrange $o 1 end]
			set opt [pget options($lastopt)]
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

		# This is -option {= config speed} - means set option to exact value
		if { $f2 == "= " } {
			# Reset option (replace existing value)
			set options($lastopt) [string range $o 2 end]
			continue
		}

		# if nomoreoptions, treat values starting from "-" as normal values,
		# until the end of the arguments in this command. It means that there
		# can be no more options passed in this command call.
		if { !$nomoreoptions && [string index $o 0] == "-" } {
			set lastopt [string range $o 1 end]
			set lastopt [UnaliasOption $lastopt]
			continue
		}

		lappend options($lastopt) $o
	}

	if { $query != "" } {
		# This means there was a query without a mask. So return everything as it goes.
		return [dict:at $agv_db($target) $query]
	}

	vlog "Target: $target {[array get options]}"

	set agv_db($target) [array get options]
}

proc ag {target args} {
	return [AccessDatabase agv::target $target {*}$args]
}

proc ag-info {filename args} {
	return [AccessDatabase agv::fileinfo $filename {*}$args]
}


proc ProcessSources target {

	set db $agv::target($target)

	set sources [dict:at $db sources]
	set objects [dict:at $db objects]
	set rules [dict:at $db rules]
	set frameworks [dict:at $db frameworks]

	set used_langs ""

	vlog "Performing general processing for program '$target':"

	set hdrs ""
	foreach s $sources {

		set info [pget agv::fileinfo($s)]
		set lang [dict:at $info language]

		if { $lang == "" } {
			# Now identify the programming language
			set lang [IdentifyLanguage $s]
			vlog " --- Audodetecting language for $s: $lang"
			dict set agv::fileinfo($s) language $lang
		}


		dict lappend used_langs $lang $s

		# Check if you have depends declared explicitly. If so, use them.
		set info [pget agv::fileinfo($s)]
		if { [dict exists $info includes] } {
			vlog " --- Explicit include declaration for '$s' - not generating"
			set deps [concat $s [dict get $info includes]]
		} else {
			vlog " --- Include info not found for '$s' - using gendep to generate:"
			set cflags [CompleteCflags $db $lang]
			set deps [GenerateDepends $lang $cflags $s]
			# Write them back to the database
			dict set agv::fileinfo($s) includes [lrange $deps 1 end] ;# skip the source itself
		}
	}

	ExecuteFrameworks $target compile-pre $sources

	foreach s $sources {

		set info [pget agv::fileinfo($s)]
		set lang [dict:at $info language]

		set o [file rootname $s].ag.o
		vlog " ... processing $s (language $lang) --> $o"

		set deps [concat $s [dict get $info includes]]

		# Generate rule for the target
		set rule [GenerateCompileRule $db $lang $o $s $deps]

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

		set hsufs [agv::p::GetHeaderSuffixes $lang]

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


	set lang [agv::p::GetCommonLanguage [dict keys $used_langs]]
	dict set db language $lang

	# Write back the database
	dict set db sources $sources
	dict set db objects $objects
	dict set db rules $rules

	set agv::target($target) $db

	ExecuteFrameworks $target compile-post $objects
}

proc Process:program target {

	set outfile $target$agv::exe

	ProcessCompileLink program $target $outfile
	Process:phony $target
}

proc Process:library target {

	# XXX THIS IS TRIAL ONLY!
	# The exact filename should be prepared basing on
	# the current OS and settings.
	set outfile lib$target.a

	ProcessCompileLink library $target $outfile
	Process:phony $target
}

proc GenerateInstallCommand {cat outfile prefix {subdir ""}} {
	set icmd ""

	switch -- $cat {
		noinst {
			# Nothing.
		}

		default {
			# Try to use the directory marked for particular category
			set bindir [subst -nocommands [dict:at $agv::profile(default) installdir:$cat]]
			if { $bindir != "" } {
				if { $subdir != "" } {
					set bindir [file join $bindir $subdir]
				}
				set icmd "\tinstall $outfile $bindir"
			} else {
				puts stderr "+++AG WARNING: No installdir for -category $cat - can't install $outfile"
			}
		}
	}

	return $icmd
}

proc GenerateInstallTarget:program {target prefix} {

	set db $agv::target($target)
	set outfile [dict:at $db filename]
	set icmd [GenerateInstallCommand [dict:at $db category] $outfile $prefix]
	if { $icmd == "" } {
		return
	}

	set itarget install-$target
	dict set db rules $itarget [list $outfile \n$icmd]
	dict set db phony $itarget ""
	# Ok, ready. Write back to the database
	set agv::target($target) $db
}

proc GenerateInstallTarget:library {target prefix} {
	set db $agv::target($target)

	set libfile [dict:at $db filename]
	set libicmd [GenerateInstallCommand [dict:at $db category] $libfile $prefix]

	set cmds ""

	# Generate also installation for headers
	set hdr [dict:at $db headers]
	foreach h $hdr {
		set hcmd [GenerateInstallCommand include $h $prefix [dict:at $db headers-subdir]]
		append cmds $hcmd\n
	}

	set itarget install-$target
	dict set db rules install-$target-archive [list $libfile \n$libicmd\n]
	dict set db phony install-$target-archive ""

	dict set db rules install-$target-headers [list {*}$hdr \n$cmds]
	dict set db phony install-$target-headers ""

	dict set db phony install-$target [list install-$target-archive install-$target-headers]

	# Ok, ready. Write back to the database
	set agv::target($target) $db
}

proc ProcessCompileLink {type target outfile} {

	ProcessSources $target

	# If the target is program, then you need
	# to generate rules that compile all sources
	# into *.o files, then link them together
	# using either global libraries or packages.
	set db $agv::target($target)
	set rules [dict:at $db rules]
	set phony [dict:at $db phony]

	# ok, we have all sources processed.
	# Now we need to generate the rule for
	# main program.

	# Collect libraries.
	# Turning packages from pkg-config or whatever
	# other systems should be done before calling this
	# function.

	set rule [GenerateLinkRule:$type $db $outfile]
	dict set rules $outfile $rule

	# Add a phony rule that redirects the symbolic name to physical file,
	# in case they differ (in future, this can be due to them being in different directories)

	set phony ""
	if { $outfile != $target } {
		vlog "Adding phony $target -> $outfile (because they differ)"
		dict set phony $target $outfile
	}
	dict set db filename $outfile

	# Ok, ready. Write back to the database
	dict set db rules $rules
	dict set db phony $phony
	set agv::target($target) $db

	set prefix [pget agv::prefix [dict:at $agv::profile(default) prefix]]
	if { $prefix == "" } {
		puts stderr "WARNING: 'prefix' not found - not generating install targets"
	} else {
		GenerateInstallTarget:$type $target $prefix
	}

	ExecuteFrameworks $target complete
}

proc Process:phony target {
	# Do the general depends processing
	set phony [dict:at $agv::target($target) phony]
	set deps [dict:at $agv::target($target) depends]

	# XXX Unclear as to why this is done only if phony is not yet set
	# with dependency
	if { [dict:at $agv::target($target) phony $target] == "" } {
		dict set agv::target($target) phony $target $deps
	}
}

proc CompleteCflags {db lang} {

	# General cflags for the target
	set cflags [dict:at $db cflags]

	# General language cflags applicable for all targets
	# For example, definitions will be collected here for -D option
	# The value of agv::cflags($lang) will be also included
	set lang_cflags [GenerateCompileFlags $lang]

	vlog " ... target's cflags: $cflags"
	vlog " ... lang($lang) cflags: $lang_cflags"

	append cflags " $lang_cflags"

}

proc GenerateCompileRule {db lang objfile source deps} {

	vlog "Generating compile rule for '$objfile':"

	set cflags [CompleteCflags $db $lang]

	# The rule should be formed as for make.tcl
	# except the 'rule' command and the target name.
	# So: source-deps... command-to-compile

	set compiler [dict get $agv::profile($lang) compile]
	set oflag [dict get $agv::profile($lang) compile_oflag]

	set command "$compiler $cflags $source $oflag $objfile"

	set rule "$deps {\n\t$command\n}"
}

proc GetTargetFile target {
	set dir [file dirname $target]
	return [file join $dir [dict:at $agv::target($target) filename]]
}

proc GetDependentLibraryTargets target {
	set libs ""
	foreach d [dict:at $agv::target($target) depends] {
		if { ![info exists agv::target($d)] } {
			error "Target '$d' (dependency of [dict:at $db name]) is not defined"
		}

		if { [dict:at $agv::target($d) type] == "library" } {
			lappend libs [GetTargetFile $d]

			# Recursive call
			# XXX consider unwinding - recursion in Tcl is limited and may
			# result in internal error!
			lappend libs {*}[GetDependentLibraryTargets $d]
		}
	}

	return $libs
}

proc GenerateLinkRule:program {db outfile} {

	set lang [dict:at $db language]
	set objects [dict:at $db objects]
	set ldflags [dict:at $db ldflags]

	set linker [dict get $agv::profile($lang) link]
	set oflag [dict get $agv::profile($lang) link_oflag]

	# Check dependent targets. If this target has any dependent targets of type library,
	# add its library specification to the flags.
	set libs [GetDependentLibraryTargets [dict:at $db name]]

	vlog "Generating link rule for '$outfile' ldflags: $ldflags libs: $libs"

	set command "$linker $objects $oflag $outfile $libs $ldflags"

	set rule "$objects {\n\t$command\n}"

	return $rule
}

# XXX ONLY FOR TESTING!!!
# This version creates just the static library out of given sources
# NOTE: static libraries are just archives with *.o files, they don't
# have dependencies - at worst they will be resolved at link time with
# dynamic libraries or executables.
proc GenerateLinkRule:library {db outfile} {
	set objects [dict:at $db objects]
	set command "ar rcs $outfile $objects"
	set rule "$objects {\n\t$command\n}"
	return $rule
}

proc silvercat-recog-line {} { return "# Generated by Silvercat" }

proc ag-do-genrules target {

	# Default target is all.
	# This is not as a "standard for makefiles" that it is
	# normal to define "all" as first target. The "all"
	# target is a special name for silvercat and it is
	# always available as a target having everything else
	# as dependency.
	if { $target == "" } {
		set target all
	}

	# Check existing makefile
	if { [file exists Makefile.tcl] } {
		set fd [open Makefile.tcl r]
		set firstline [string trim [gets $fd]]
		if { $firstline != [silvercat-recog-line] } {
			puts stderr "+++ ERROR: The Makefile.tcl file to be generated exists"
			puts stderr "+++ and it's not a Silvercat-generated file. If this Makefile.tcl"
			puts stderr "+++ is something you don't need, please delete it first."
			return 1
		}
	}

	# Complete lacking values that have to be generated.
	if { ![agp-prepare-database $target] } {
		puts stderr "Failed to prepare database for '$target'"
		return 1
	}

	vlog "Database for '$target' completed. Generating Makefile"

	set fd [open Makefile.tcl w]

	# Print header
	# Just don't print this in the sub-calls
	puts $fd [silvercat-recog-line]
	puts $fd "# DO NOT MODIFY THIS FILE - or remove the above comment line."
	puts $fd "# This file is generated and its contents will be overwritten.\n"



	set ok [GenerateMakefile $target $fd]
	if { !$ok } {
		puts $fd "# XXX THIS MAKEFILE IS INVALID. Please check errors and regenerate"
	}

	close $fd
	return [expr {$ok ? 0 : 1}]
}

proc GenerateMakefile {target fd} {

	vlog "Generating makefile for $target"

	set rules [dict:at $agv::target($target) rules]

	# Rules is itself also a dictionary.
	# Key is target file, value is dependencies and command at the end.

	foreach {tarfile data} $rules {
		puts $fd "rule $tarfile $data"
	}

	# First rules, then phony. Later phonies may override earlier rules.
    set phony [dict:at $agv::target($target) phony]

	if { $phony != "" } {
		foreach {rule deps} $phony {
			puts $fd "phony $rule $deps"
		}
	}

	puts $fd ""

	# Now generate everything for the dependent targets
	set deps [dict:at $agv::target($target) depends]

    vlog "Makefile generator: will generate sub-rules for deps: $deps"
    foreach d $deps {
    	GenerateMakefile $d $fd
    }
    vlog "Makefile generator: finished sub-rules for deps"

	set cleanname $target-clean
	if { $target == "all" } {
		set cleanname clean
	}

	# Clean rule
	puts $fd "rule $cleanname {
	!tcl autoclean $target
}"
	puts $fd "phony $cleanname"

	return true
}

proc ag-do-make {target} {

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

	return 0
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
		dict set agv::target($target) frameworks $frameworks
	}

	if { ![ExecuteFrameworks $target prepare] } {
		return false
	}

	vlog " ... Processing '$target' as '$type'"
	Process:$type $target
	
	vlog "DATABASE for '$target' AFTER PROCESSING:"
	foreach k [dict keys $agv::target($target)] {
		if { $k == "rules" } {
			# Show rules special way for clarity
			vlog "   -rules:"
			set rules [dict get $agv::target($target) $k]
			foreach {ofile rule} $rules {
				set rule [string map {\n " <CR> "} $rule]
				vlog "      $ofile : $rule"
			}
		} else {
			vlog [join [list "   -$k:" [dict get $agv::target($target) $k]]]
		}
	}

	vlog "PROCESSING DEPENDS of $target"

	foreach dep [dict:at $agv::target($target) depends] {
		if { ![agp-prepare-database $dep] } {
			return false
		}
	}

	vlog "END DEPENDS OF $target"
	return true
}

proc ag-subdir args {
	if { [llength $args] == 1 } {
		set args [lindex $args 0]
	}

	lappend agv::p::directories {*}$args
}

proc ag-do-help {args} {
	puts "Usage: [file tail $::argv0] genrules <target>"
	return 1
}

package provide ag 0.8

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
exit [ag-do-$arg1 [lrange $argv 1 end]]


}
