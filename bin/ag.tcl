#!/usr/bin/tclsh


# Import and fix several utilities from agmake.
namespace eval mkv {
	namespace eval p {

		# Prologue
		set me [info script]
		set here [file dirname $me]

		set gg_makepath $here/agmake
		
		# Import make's utilities
		source $here/mkv.p.utilities.tcl
		source $here/mkv.p.builtin.tcl

		namespace export {*}$public_export_util {*}$public_export_builtin
		set public_import ""
		foreach n [concat $public_export_util $public_export_builtin] {
			lappend public_import "mkv::p::$n"
		}
	}
	proc MAKE {} {
		return [file normalize $::mkv::p::gg_makepath]
	}

}

namespace import {*}$mkv::p::public_import

namespace eval agv {
	set version 0.9 ;# just to define something

	set gg_agpath [file normalize [info script]]
	variable runmode ""

	proc AG {} {
		set options ""
		if { $mkv::p::verbose } {
			append options " -v"
		}
		if { $::ag_debug_on } {
			append options " -d"
		}

		return $agv::gg_agpath$options
	}

	variable target
	namespace export target

	variable foreign
	namespace export foreign

	# Supporting directories
	variable directories
	namespace export directories

	# This variable isn't used to mark that rules for targets
	# have been generated (this is mkv::generated). This agv::generated
	# is used to collect files that have been generated together
	# with Makefile.tcl, so if they are lacking, you should run reconfigure.
	variable generated
	namespace export generated

	namespace eval p {

		# Prologue
		set me [info script]
		set here [file dirname $me]

		# Ingredients

		source $here/agv.p.utilities.tcl
		source $here/agv.p.builtin.tcl
		source $here/agv.p.builtin-profiles.tcl
		source $here/agv.p.builtin-frameworks.tcl

		# Variable and procedures that should be exported from
		# the silverball file to the makefile.
		set exported_proc ""
		set exported_var ""

		pinit profile_overrides ""

		# List of internal variables to be derived
		# by a subdirectory.
		set subdir_export {
			agv::p::profile_overrides
		}
	}

	set exe $p::exefor([info sharedlibextension])
	namespace export exe

	# Profile is an array indexed by language name.
	variable profile
	namespace export profile

	# Install prefix.
	# Will be set to the profile's default, unless overridden.
	proc prefix {} {
		return [ag-profile general ?install:prefix]
	}

	proc path {pathspec} {
		return [ResolveOutput1 $pathspec s]
	}

	# Per-file set information
	variable fileinfo
	namespace export fileinfo

	# Private variable of genrules. Marks targets for which
	# the rules have been already generated.
	variable genrules_done ""
	namespace export genrules_done

	variable generated_files ""
	namespace export generated_files

	variable srcdir
	variable statedir .
	namespace export { srcdir statedir }

	variable libpath ""
	namespace export libpath

	# This is for user options; the build file defines the
	# options and their defaults through the ag-option command.
	# The user may override them through the long option.
	variable useropt ""
	variable useropt_filter ""
	variable useropt_override ""
	namespace export useropt
	namespace export useropt_filter
	namespace export useropt_override
}

namespace import agv::p::GenFileBase

proc RealSourcePath target {

	# return [prelocate [file join $agv::srcdir $target]]

	#vlog "*** RESOLVING '$target' in $agv::srcdir"

	# THIS IS "if" - repeatable by continue
	while { [string first / $target] } {
		set rem [lassign [file split $target] first]
		if { $first == "." } {
			#vlog " ... explicit current directory: $target -- cutting off dir and repeating"
			set target [file join {*}$rem]
			continue
			# We have a ./FILENAME
			#return $target
		}

		# Check if the path is absolute already
		# If so, return it as is
		if { [file normalize $target] == $target } {
			#vlog " ... explicit absolute directory: $target"
			return $target
		}

		break ;# should be a repeatable if-condition
	}

	set dir $agv::srcdir
	if { $dir == "." } {
		set dir ""
	}

	# In all other cases, return the path
	# readjusted to srcdir
	set out [file join $dir $target]
	#vlog " ... readjusted: $out"
	return $out
}

proc FixShadowPath val {
	set rp [RealSourcePath $val]

	vlog "FixShadowPath: real-source($val) = $rp"
	vlog "... prelocate to $agv::builddir up to $agv::toplevel"

	return [prelocate $rp $agv::builddir $agv::toplevel]
}

proc IsSubTarget target {
	set p [file split $target]
	if { [llength $p] != 1 } {
		return true
	}

	return false
}

proc GetUnifiedProfileFlags {lang type} {
	return [GetUnifiedProfile-$type $lang]
}

proc TranslateValue {value map} {
	# This translates value according to the map:
	# - if the value is among keys, the associated value is returned
	# - if the value is among values, it's returned as is
	# - if both key and value are empty at some iteration (should be the last one), the value is returned as is anyway
	# - otherwise an empty value is returned

	foreach {key val} $map {
		if { $value == $key } {
			return $val
		}

		if { $value == $val } {
			return $val
		}

		if { $key == "" && $val == "" } {
			return $value
		}
	}

	return ""
}

proc TranslateFlags { lang flagmap mainkey {order append} } {
	# flagmap is expected to be:
	# flagname flagvalues... ...
	# flagname should be without "-". Allowed flagnames are:
	# - cflags (to be passed through)
	# - defines
	# - incdir
	# - libdir
	# - std

	# As a result it should return the exact value to be set to target's cflags

	set flag(defines) [pget agv::profile($lang).defineflag]
	set flag(incdir) [pget agv::profile($lang).incdirflag]
	set flag(libdir) [pget agv::profile($lang).libdirflag]
	set flag(std) [pget agv::profile($lang).std_option]

	set transmap(std) [pget agv::profile($lang).std_values]

	# Set up the existing cflags
	set outflags_old [pget flagmap.$mainkey]
	set outflags_new ""

	foreach flagtype [array names flag] {
		foreach val [dict:at $flagmap $flagtype] {
			if { [info exists transmap($flagtype)] } {
				set traval [TranslateValue $val $transmap($flagtype)]
				if { $traval == "" } {
					error "Flag -$flagtype: value '$val' is invalid"
				}
				set val $traval
			}

			# Transform the path into relative-topdir-based, if the
			# flag is *dir (currently: incdir, libdir, but potential
			# others may be added, too).
			if { [string match *dir $flagtype] } {
				$::g_debug "RESOLVING '$flagtype': $val"
				set val [ResolveOutput $val s]
				$::g_debug " ... : $val"
			}
			set e $flag($flagtype)$val
			if { $e ni $outflags_old && $e ni $outflags_new } {
				lappend outflags_new $e
			}
		}
	}

	if { $order == "prepend" } {
		return [concat $outflags_new $outflags_old]
	} else {
		return [concat $outflags_old $outflags_new]
	}
}

proc GetUnifiedProfile-cflags lang {

	# XXX In Tcl 8.5, dict filter/key can get ONLY ONE KEY PATTERN.
	# So, do filtering multiple times anyway :(

	set flagmap [dict:filterkey $agv::profile($lang) cflags defines incdir std]
	set cflags [TranslateFlags $lang $flagmap cflags]

	$::g_debug " --- Profile-defined cflags for $lang: $cflags"

	return $cflags
}

proc GetUnifiedProfile-ldflags lang {

	set flagmap [dict:filterkey $agv::profile($lang) ldflags libdir std]
	set ldflags [TranslateFlags $lang $flagmap ldflags prepend]

	$::g_debug " --- Profile-defined ldflags for $lang: $ldflags"

	return $ldflags
}


# This procedure transfers the high-level flags into low-level.
# The flags added as 'defines' and 'incdir' are transformed into
# 'cflags', and 'libdir' into 'ldflags', with the use of appropriate
# compiler options for passing these data, as retrieved from
# the profile (e.g. for gcc it's -D for 'defines', -L for 'libdir'
# etc.).
proc ProcessFlags target {
	set db $agv::target($target)

	set lang [dict get $db language]

	set flagmap [dict:filterkey $db cflags defines incdir std]

	dict set agv::target($target) cflags [TranslateFlags $lang $flagmap cflags]

	set flagmap [dict:filterkey $db ldflags libdir std]
	dict set agv::target($target) ldflags [TranslateFlags $lang $flagmap ldflags prepend]

# --- 	set defines_flag [pget agv::profile($lang).defineflag]
# --- 	set incdir_flag [pget agv::profile($lang).incdirflag]
# --- 	set libdir_flag [pget agv::profile($lang).libdirflag]
# --- 
# --- 	set defines [dict:at $db defines]
# --- 	set incdir [dict:at $db incdir]
# --- 	set libdir [dict:at $db libdir]
# --- 
# --- 	set cflags [dict:at $db cflags]
# --- 
# --- 	foreach flagtype {defines incdir} {
# --- 		foreach val [dict:at $db $flagtype] {
# --- 			set e [set ${flagtype}_flag]$val
# --- 			if { $e ni $cflags } {
# --- 				lappend cflags $e
# --- 			}
# --- 		}
# --- 	}
# --- 
# --- 	set ldflags [dict:at $db ldflags]
# --- 	set need_ldflags ""
# --- 
# --- 	foreach val [dict:at $db libdir] {
# --- 		set e ${libdir_flag}$val
# --- 		if { $e ni $ldflags } {
# --- 			lappend need_ldflags $e
# --- 		}
# --- 	}
# --- 
# --- 	if { $need_ldflags != "" } {
# --- 		set ldflags [concat $need_ldflags $ldflags]
# --- 	}
# --- 
# --- 	#puts stderr "($target) Extra cflags from special options: $cflags (from defines: $defines incdir: $incdir lang: [dict:at $db language])"
# --- 
# --- 	dict set agv::target($target) cflags $cflags
# --- 	dict set agv::target($target) ldflags $ldflags
}

proc ShellWrap arg {
	#puts stderr " --- --- Would wrap: '$arg'"
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
		append out "[ShellWrap $a] "
	}

	#puts stderr " --- --- Wrapped: '$out'"
	return $out
}


proc FindSilverFile {agfile {agdir ""}} {

	set possible_agfiles {Makefile.ag.tcl Makefile.ag makefile.ag.tcl makefile.ag Silverball silverball}

	if { $agfile == "" } {

		vlog "*** Trying to find a standard silverfile in $agdir"

		foreach agfile [list {*}$possible_agfiles .] {
			# "dot" always exists - it's the current directory
			# buf even if it accidentally doesn't exist, it doesn't matter!
			if { [file exists [file join $agdir $agfile]] } break
		}

		if { $agfile == "." } {
			puts stderr "The Silvercat file not found in '[file normalize $agdir]' among:"
			puts stderr $possible_agfiles
			puts stderr "Use -f <silvercat file> to set it explicitly"
			return
		}

		vlog "... Found: $agfile"
	} else {

		vlog "*** Checking file '$agfile' in '$agdir'"
		set agfile [file join $agdir $agfile]
		if { ![file exists $agfile] } {
			puts stderr "File not found: $agfile"
			return
		}
	}

	return $agfile
}

# XXX This should be platform-dependent!
proc CreateLibraryFilename {target type oldname} {

	# The 'oldname' may be:
	# - empty: in this case just return the given name
	# - have only a rootname - then just use it in the template
	# - be a full filename - then check if correct extension

	set rootname $oldname
	if { $rootname == "" } {
		set rootname $target
	} else {
		set rootname [file rootname $rootname]
	}

	set proflang [dict:at $agv::target($target) language]
	if {$proflang == ""} {
		set proflang general
	}

	if { $type == "shared" } {
		set form [ag-profile $proflang ?form:sharedroot]
		set outname [pdip $form $target][info sharedlibextension]
	} elseif { $type == "static" } {
		set form [ag-profile $proflang ?form:archive]
		set outname [pdip $form $target]
	} else {
		error "Invalid libspec '$type' for target '$target': use 'static' or 'shared'"
	}

	# Confront outname with oldname
	if { [file extension $oldname] != "" } {
		if { [file extension $outname] != [file extension $oldname] } {
			error "For target '$target' output '$oldname' has invalid extension; should be like '$outname'."
		}

		# If the extensions match, accept the oldname without questions
		return $oldname
	}

	return $outname
}

# This function joins two lists into one list, keeping
# the elements unique and preserving the order of elements.
# There may be expected some elements occurring in both lists.
# The resulting list returns the elements from both lists, where the
# original order of elements is preserved.
# For the best expected results, the input lists should:
#  - contain only unique elements (one element cannot repeat more than once in one list)
#  - elements common for both lists should be in the same order towards each other
# If these conditions are not true, then:
#  - if an element occurs in the same list more than once, only the first occurrence is
#    passed to the output list and only this element's stability is preserved
#    Example: StableIntersection {a b a} {b c d} -> {a b c d}
#  - if two elements occur in both lists, but in different order, the order of
#    the first list is preserved.
#    Example: StableIntersection {a b d c} {a c e d} -> {a b d c e}
proc StableIntersection {l2 l1} {

	set out ""

	foreach e1 $l1 {
		set ip [lsearch $l2 $e1]
		if { $ip == -1 } {

			# Add this element only if it wasn't added before.
			# This can only happen in case when the common elements
			# weren't in the same order (the order of l2 is actually
			# preserved).
			if { [lsearch $out $e1] == -1 } {
				lappend out $e1
			}
			continue
		}

		# If found, then we move all elements
		# before the found one to the output stream
		if { $ip > 0 } {
			for {set i 0} {$i < $ip} {incr i} {
				set e2 [lindex $l2 $i]
				if { [lsearch $out $e2] == -1 } {
					lappend out $e2
				}
			}

			# Remove all these elements preceding the found one,
			# together with that one.
		}
		if { [lsearch $out $e1] == -1 } {
			lappend out $e1
		}
		set l2 [lreplace $l2 0 $ip]
	}

	# Should there be anything remaining in the l2 list,
	# add it at the end
	foreach e2 $l2 {
		if { [lsearch $out $e2] == -1 } {
			lappend out $e2
		}
	}

	return $out
}

proc CreateDepGenCommand {lang cflags source} {
	$::g_debug "Extracting ingredients of '$source' (cflags: $cflags)"

	set gendep [dict:at [pget agv::profile($lang)] gendep]
	if { $gendep == "" } {
		puts stderr "+++ Ag/ERROR: 'gendep' undefined for language $lang"
		error "Please complete profile for language $lang or define includes explicitly"
	}

	set wrapped_flags [ShellWrapAll $cflags]

	set cmd "$gendep $wrapped_flags $source"
	return $cmd
}

proc GenerateDepends {lang cflags source} {

	set cmd [CreateDepGenCommand $lang $cflags $source]
	puts stderr "Generating dependencies for: $source"
	vlog "Dep command: $cmd"

	# Run the command to generate deps
	#puts "Command: "
	#foreach c $cmd {
#		puts " --> $c"
#	}

	# The command should be run originally in the source directory
	if { [catch {
				set wd [pwd]
				cd $agv::srcdir
				$::g_debug "<Entering> [pwd]"
				set deps [exec {*}$cmd]
				cd $wd
				$::g_debug "<Back in> [pwd]"
			} error] } {
		puts stderr "ERROR: dependency generation command failed:\n$error"
		puts stderr "If this is because of nonexistent HEADER file, use:\n"
		puts stderr "\tag-info <cfile> -includes <hfile>'\n"
		puts stderr "to prevent autogeneration, or use -depspec cached."
		error "Command failed: $cmd"
	}

	set deps [mkv::p::TranslateDeps $deps]

	# Rules are generated in the convention of "make".
	# Make them a plain list, as needed for "make.tcl"
	#set deps [string map { "\\\n" " " } $deps]
	# Drop the *.o target, we don't need it.
	#set deps [lrange $deps 1 end]
	$::g_debug "Resulting deps: $deps"

	return $deps
}

proc ExecuteFrameworks {target step args} {
	set frameworks [dict:at $agv::target($target) frameworks]
	$::g_debug " ... Frameworks/$step: $frameworks ($args)"
	foreach frm $frameworks {
		if { [string first : $frm] == -1 } {
			# Treat it as a builtin, that is agv::p::fw::$NAME
			set frm "::agv::p::fw::$frm"

			if { ![namespace exists $frm] } {
				error "No such BUILTIN framework '$frm'\n*** (external frameworks must be Tcl namespace names)"
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
			error "Error executing framework '$procname' on '$target':\n$err"
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
	# If no dot found, return "phony".
	# (program may be good, but it's not a good default)

	# XXX This is true only on POSIX.
	# Mind the $agv::exe value as a suffix.
	# On Windows and Mac the no-dot files should
	# be reported as Unknown.

	if { [string first $target .] == -1 } {
		return phony
	}

	set ext [file extension $target]

	if { $ext == [info sharedlibextension] } {
		return runtime
	}

	if { $ext in {.lib .a} } {
		return archive
	}

	# names with dot in the name, but having no
	# recognizable extension, return empty string
	# as unrecognized.
	return
}

proc ag-require args {

	set optional 0

	if { [lindex $args 0] == "-optional" } {
		set args [lrange $args 1 end]
		set optional 1
	}

	set ibrk 0
	foreach a $args {
		foreach p $agv::libpath {
			if { $ibrk } {
				break
			}
			set n [file join $p $a]
			if { [file exists $n] } {
				source $n
				set ibrk 1
				continue
			}
		}
		if { $ibrk } {
			set ibrk 0
			continue
		}

		# This means it wasn't found
		if { $optional } {
			return false
		}
		set in [expr { ($agv::libpath == "") ? ": no AG_LIB_PATH nor ../lib or ../share found" : " in: $agv::libpath" }]
		error "Silvercat package '$a' not found$in"
	}

	return true
}

proc ag-profile {name args} {
	if { $args == "" } {
		return [InstallProfile $name]
	}

	# Otherwise "name" is:
	# - general (apply change to all languages)
	# - lang or {lang1 lang2} to apply to selected languages

	if { $name == "general" } {

		set results [AccessDatabase agv::profile default {*}$args]
		if { [string index [lindex $args 0] 0] == "?" } {
			# It was a query, so stop on querying the value for general.
			return $results
		}
		# Otherwise continue and spread the requests to all other languages.
		# Get all language names except "default" and "structure".
		set name [plremove [array names agv::profile] default structure]
	}

	if { $name == "" } {
		set name default
	}

	set results ""

	foreach lang $name {
		lappend results {*}[AccessDatabase agv::profile $lang {*}$args]
	}

	$::g_debug "Updated profile($name): $agv::profile([lindex $name 0])"
	return $results
}

proc ag-option {name args} {

	# This command requires that the name
	# is always defined with -- in front and it is not allowed
	# to have the config prefix.
	# The name is notified in the array without the double-dash.
	if {[string range $name 0 1] != "--"} {
		error "ag-option: option name must start with -- part"
	}

	set name [string range $name 2 end]

	# We use args in order to not use the mechanism of
	# checking the default value that doesn't have a
	# "null form", while we must allow also an empty string
	# and whatever other kind of value for the "default value".

	if {$args == ""} {
		# Single name specified - requested to obtain the current value.
		# Translate the error

		if {![dict exists $agv::useropt $name]} {
			error "ag-option: no such option --$name"
		}

		return [dict get $agv::useropt $name]
	}

	set others [lassign $args deflt filter]
	if {$others != ""} {
		error "Usage: ag-option --<name> <default-value> ?filter?"
	}

	lassign [split $name -] prefix

	if {$prefix in {avail disable enable have use}} {
		error "ag-option: option with a prefix reserved for config is not allowed"
	}

	if {$filter != ""} {
		set fi [string index $filter 0]
		if {$fi != "/" && $fi != "~" && $filter ni {number bool keyword}} {
			error "ag-option: filter keywords allowed: number, bool, keyword or /regexp or ~globmatch"
		}
		dict set agv::useropt_filter $name $filter
	}

	dict set agv::useropt $name $deflt

	# Now check if the user has already provided the value
	# for the option.

	if {[dict exists $agv::useropt_override $name]} {
		lassign [OverrideUserOption $name [dict get $agv::useropt_override $name]] have msg
		if {!$have} {
			# Not possible at this place actually, so this is simply an error
			error "ag-option: $msg"
		}
	}
}

# This function will be called in response to passing a --longoption
# to the command line. If this resolves to an option, it will be set
# and consumed, otherwise tried another possibility.
proc OverrideUserOption {name value} {
	if {![dict exists $agv::useropt $name]} {
		return false
	}

	# Check if the value matches the filter
	# If no filter, just set the value as is.
	if {![dict exists $agv::useropt_filter $name]} {
		dict set agv::useropt $name $value
		return true
	}

	# Check if this is a correct filter
	# Filter types can be:
	#
	# - regular expression - starts from /
	# - glob match - starts from ~
	# - special keyword:
	#   - number: accepts a number, specified as hex/dec with dot etc.
	#   - bool: standard Tcl boolean value
	#   - keyword: same as /[a-zA-Z_][a-zA-Z_0-9]*

	set filter [dict get $agv::useropt_filter $name]
	if {[string index $filter 0] == "/"} {
		set rex "^[string range $filter 1 end]\$"
		if {![regexp $rex $value]} {
			return [list false "The --$name option argument must match regexp '$rex'"]
		}
	} elseif {[string index $filter 0] == "~"} {
		set mat [string range $filter 1 end]
		if {![string match $mat $value]} {
			return [list false "the --$name option argument must match pattern '$mat'"]
		}
	} else {
		switch -- $filter {
			number {
				if {![string is entier $value] && ![string is double $value]} {
					return [list false "the --$name option argument must be a number"]
				}
			}

			bool {
				if {![string is bool $value]} {
					return [list false "the --$name option argument must be a boolean"]
				}
			}

			keyword {
				if {![regexp {^[a-zA-Z_][a-zA-Z_0-9]*$} $value]} {
					return [list false "the --$name option argument must be a keyword string"]
				}
			}
		}
	}

	dict set agv::useropt $name $value
	return true
}

proc InstallProfile {name} {

	if { $name in {default structure} } {
		error "Invalid profile name (keyword)"
	}

	if { ![dict exists $agv::p::profiles $name] } {
		error "No such profile: $name"
	}

	set globpdefs ""
	if { ![array exists agv::profile] } {
		set globpdefs [dict get $agv::p::profiles default]
		$::g_debug "FRESH PROFILE: adding defaults: $globpdefs"
	} else {
		# Check if this 'existing profile' consists of only name.
		# If so, still apply the defaults
		if { [dict remove [pget agv::profile(default)] name] == "" } {
			set globpdefs [dict get $agv::p::profiles default]
			$::g_debug "EXISTING PROFILE, but name only: - adding default: $globpdefs"
		} else {
			$::g_debug "EXISTING PROFILE: [array get agv::profile]"
		}
	}

	set newprof [dict get $agv::p::profiles $name]

	$::g_debug "ADDING PROFILE CONTENTS: $newprof"
	$::g_debug "PROFILE OVERRIDES: [pget agv::p::profile_overrides]"

	# Merge manually - official merging would overwrite keys and therefore
	# possibly delete sub-keys, while we want
	# to merge sub-keys.
	array set tmp_prof $globpdefs

	foreach {key val} $newprof {
		# Skip keys that exist in "profile override" array
		set tmp_prof($key) [dict merge [pget tmp_prof($key)] $val [pget agv::profile.$key]]
	}

	set prof [array get tmp_prof]
	$::g_debug "MERGED: $prof"

	# Remove any "comments" that might have been put there :)
	set prof [dict remove $prof #]
	set o ""
	dict for {k v} $prof {
		lappend o $k [dict remove $v #]
	}
	set prof $o

	# Update the target key, if possible

	if {![dict exists $prof default target]} {
		# Target not known, so check if components to obtain it exist
		set vspec [dict:at $prof default version]
		set tmark [dict:at $prof default targetspec]
		if {$vspec != "" && $tmark != ""} {
			set vline [exec {*}$vspec |& grep $tmark]
			set tname [string trim [string range $vline [string length $tmark] end]]
			dict set prof default target $tname
		}
	}

	# Now merge every language item with default.
	# Leave the default untouched, however.
	foreach lng [dict keys $prof] {
		if { $lng == "default" } {
			continue
		}

		dict set prof $lng [dict merge [dict:at $prof default] [dict:at $prof $lng]]
	}

    foreach {okey oval} [pget agv::p::profile_overrides] {
    	lassign [split $okey /] lng lky
    	if { $lky == "" } {
    		set lky $lng
    		set lng default
    	}
    
    	$::g_debug "OVERRIDING \[$lng $lky\]: $oval"
    	dict set prof $lng $lky $oval
    }

	$::g_debug "InstallProfile: $prof"

	array set agv::profile $prof
}

proc UnaliasOption alias {
	switch -- $alias {
		s { return sources }
		h { return headers }
		nh { return noinst-headers }
		fw { return frameworks }
		hidir { return headers-installdir }
		idir { return installdir }
		o { return output }
		I { return incdir }
		D { return defines }
		L { return libdir }
		libs { return ldflags }
		lflags { return ldflags }
		requires {
			return depends;# XXX WARNING! elements of 'depends' list must be filtered through [CheckDefinedTarget]!
		}
	}

	return $alias
}

proc DebugDisplayDatabase {array target} {
	if { !$::ag_debug_on } {
		return
	}

	upvar $array agv_db

	foreach k [dict keys $agv_db($target)] {
		if { $k == "rules" } {
			# Show rules special way for clarity
			$::g_debug "   -rules:"
			set rules [dict get $agv_db($target) $k]
			foreach {ofile rule} $rules {
				set rule [string map {\n " <CR> "} $rule]
				$::g_debug "      $ofile : $rule"
			}
		} else {
			$::g_debug [join [list "   -$k:" [dict get $agv_db($target) $k]]]
		}
	}
}

proc AccessDatabase {array target args} {

	upvar $array agv_db

	set lastopt ""

	if { [llength $args] == 1 } {
		# May contain multiple lines, so wipe out comments
		set args [lindex [puncomment $args] 0]
		# Possibly expand using variables from 3 frames up
		# (this function is always the first level call from ag-* functions)
		set args [pexpand $args 3]
	}

	set keypath ""

	set debugpath [join $target .]
	if { [llength $target] > 1 } {
		set keypath [lassign $target target]
	}

	$::g_debug " ---AC/DB $array\[$debugpath\]: $args"

	if { ![info exists agv_db($target)] } {
		# Make the database created, if not yet exists
		dict set agv_db($target) name $target
	}

	set singles [pget agv::p::keytype(single)]
	set commandlike [pget agv::p::keytype(command)]
	set uniques [pget agv::p::keytype(unique)]

	# Get old options
	set db $agv_db($target)
	array set options [dict get $db {*}$keypath]

	set query ""
	set nomoreoptions false

	foreach o $args {

		set push_front false
		if { $query != "" } {
			# This is considered argument for the query, which is a mask.
			set current [dict:at $agv_db($target) {*}$keypath $query]
			$::g_debug "AC/DB Q: keypath=$keypath query='$query'"
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
			if { $query != "" } {
				error "Query can contain only one key"
			}
			$::g_debug "...Found query request: $o"
			set query [string range $o 1 end]
			continue
		}

		if { $o == "--" } {
			set nomoreoptions true
			continue
		}

		set f2 [string range $o 0 1]
		set f3 [string range $o 0 2]
		#$::g_debug " --- --- AC/DB: f2:'$f2' f3:'$f3' o:'$o'"

		# This time it's -option {- config speed} - means remove these items
		if { !$nomoreoptions && $f2 == "- " } {
			set o [pexpand [lrange $o 1 end] 3]
			set opt [pget options($lastopt)]
			set pos ""

			$::g_debug " --- --- AC/DB: $lastopt -= $o"
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
		if {  !$nomoreoptions && $f2 == "= " } {
			# Reset option (replace existing value)

			set o [pexpand [string range $o 2 end] 3]
			$::g_debug " --- --- AC/DB: $lastopt = $o"
			set options($lastopt) $o
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

		set update_mode ""

		if { !$nomoreoptions && $f2 == "+ " } {
			set o [string range $o 2 end]
		}

		if { !$nomoreoptions && $f3 == "++ " } {
			set o [string range $o 3 end]
			set update_mode push_front
		}

		if { !$nomoreoptions && $f3 == "+- " } {
			set o [string range $o 3 end]
			set update_mode unique
		}

		if { $update_mode == "" } {
			# Depending on type.
			if { $lastopt in $singles } {
				set update_mode override
			} elseif { $lastopt in $uniques } {
				set update_mode "unique forced"
			} else {
				set update_mode push_back
			}
			# commandlike check is unnecessary - it doesn't use $update_mode.
		}

		$::g_debug "TO SET '$lastopt' (before): $o"
		set o [puncomment $o]
		set o [pexpand $o 3]
		$::g_debug "TO SET '$lastopt' (after ): $o"
		# This time it's nothing special. At least try to strip one level,
		# in case when user did -option \{value1 value2\}

		if { $lastopt in $commandlike } {

			# Do a command-like processing.
			# Keep \n intact, cumulate, merge by lines.
			# Just strip initial and terminal \n, then
			# link commands by \n.
			set len [string length $o]
			set p 0
			while { [string index $o $p] == "\n" && $p < $len } {
				incr p
			}
			set e 0
			while { [string index $o end-$e] == "\n" && $e < $len } {
				incr e
			}
			set o [string range $o $p end-$e]
			if { [pget options($lastopt)] == "" } {
				set options($lastopt) "\n"
			}
			append options($lastopt) "$o\n"

		} else {
			if { ![catch {llength $o} size] } {
				if { $size == 1 } {
					set o [lindex $o 0]
				}
				$::g_debug "Updating '$array' @ '$lastopt' mode '$update_mode' with value '$o'"
				if { $update_mode == "push_front" } {
					set options($lastopt) [concat $o $options($lastopt)]
				} elseif { $lastopt in $singles } {
					if { [info exists options($lastopt)] } {
						vlog " +++ $lastopt is expected as single - OVERRIDING existing value '$options($lastopt)' with $o"
					}
					set options($lastopt) $o
				} else {
					if { "unique" ni $update_mode || ![info exists options($lastopt)] || $o ni $options($lastopt) } {
						lappend options($lastopt) {*}$o
					} elseif { "forced" in $update_mode } {
						# unique-forced mode causes that if a key is non-uniquely
						# updated and it wasn't a requested unique update, report error
						if { [info exists agv::p::dbname($array)] } {
							set name $agv::p::dbname($array)
						} else {
							set name $array
						}
						error "ERROR: In $name:$target -$lastopt key is UNIQUE and value '$o' is already present. Use {+- $o} to prevent this error."
					}
				} 
			} else {
				# Happened to be a text not convertible to a list.
				# Well, happens. Just append to the existing value.
				if { [info exists options($lastopt)] } {
					if { $update_mode == "push_front" } {
						set options($lastopt) "$o $options($lastopt)"
					} else {
						if { $update_mode != "unique" || $o ni $options($lastopt) } {
							append options($lastopt) " $o"
						}
					}
				} else {
					set options($lastopt) $o
				}
			}
		}
		# Do nothing in case when calculating lenght resulted in exception.

		$::g_debug " --- --- AC/DB: $lastopt += {$o}"
	}

	if { $query != "" } {
		$::g_debug "... [join $keypath /]/$query result: [dict:at $agv_db($target) {*}$keypath $query]"
		# This means there was a query without a mask. So return everything as it goes.
		return [dict:at $agv_db($target) {*}$keypath $query]
	}

	$::g_debug "Updated $array: [join $target {*}$keypath] {[array get options]}"

	# Unfortunately there's no [dict set] version with no key spec that works exactly like set
	# (unlike [dict get] without keypath that just returns given value).
	if { $keypath == "" } {
		set agv_db($target) [array get options]
	} else {
		dict set agv_db($target) {*}$keypath [array get options]
	}
}

proc ag {target args} {
	# Turn target name into path-based target, if a relative
	# state directory was set.
	#$::g_debug "*** request prelocate by 'ag' for '$target'"

	# There may be problems with targets which's name start from dash
	# that concerns options handling.
	if { [string index $target 0] == "-" } {
		error "Target name cannot start from '-'"
	}

	set tar [file join $agv::statedir $target]
	set target [prelocate $tar]

	set wasthere [info exists agv::target($target)]

	set res [AccessDatabase agv::target $target {*}$args]
	# The database accessor does the whole parsing and it's
	# not possible to intersect anything here. On the other hand,
	# it doesn't matter whether we report error before or after the access.

	if { !$wasthere && ![dict exists $agv::target($target) type] } {
		if { [string index [lindex $args 0] 0] == "-" } {
			error "+++ ERROR: First access to ag '$target' must specify a type."
		} else {
			error "+++ ERROR: Target '$target' doesn't exist - invalid query"
		}
	}

	return $res
}

proc ag-info {filename args} {
	return [AccessDatabase agv::fileinfo $filename {*}$args]
}

proc agv-notify-filewrite-handler {commandstring op} {
	set others [lassign $commandstring command_open filename mode]
	if { $command_open != "open" } {
		return
	}

	if { [file exists $filename] && "w" in [split $mode ""] } {
		$::g_debug "TRACED: open w detected - $filename is considered generated"
		lappend agv::generated_files $filename
	}
}

proc ag-maf {target directory args} {
	set option ""
	foreach a $args {
		if { [string index $a 0] == "-" } {
			lappend keys($a)
			set option $a
			continue
		}

		lappend keys($option) $a
	}

	if { [info exists keys()] } {
		set filename [file join $keys()]
	} else {
		set filename ""
	}
	array unset keys ""

	set output [agv::p::MafRead $directory $filename]
	foreach key [array names keys] {
		set value ""
		foreach section $keys($key) {
			if { [dict exists $output $section] } {
				lappend value [dict get $output $section]
			}
		}
		ag $target $key {*}$value
	}

	return $output
}


proc ProcessLanguage target {
	set db $agv::target($target)

	set sources [dict:at $db sources]
	set used_langs ""

	foreach s $sources {

		set info [pget agv::fileinfo($s)]
		set lang [dict:at $info language]

		if { $lang == "" } {
			# Now identify the programming language
			set lang [IdentifyLanguage $s]
			#$::g_debug " --- Autodetecting language for $s: $lang"

			if { $lang == "" } {
				error "Can't autodetect language for source named '$s' - please specify!"
			}

			dict set agv::fileinfo($s) language $lang
		}

		if { [string match *-header $lang] } {
			puts stderr "+++ Ag/ERROR: Target:$target Source:$s"
			puts stderr "+++ Ag/ERROR: File identified as header file ($lang)"
			error "A file identified as 'header' ($lang) cannot be a source file."
		}

		dict lappend used_langs $lang $s
	}

	set lang [dict:at $db language]
	if { $lang == "" } {

		if { $used_langs != "" } {
			set lang [agv::p::GetCommonLanguage [dict keys $used_langs]]
			dict set db language $lang
		} else {
			set default_language [dict:at $agv::profile(default) language]
			if { $default_language == "" } {
				set default_language default
			}
			dict set db language $default_language
		}
	}

	# Write the database before running framework's hook
	set agv::target($target) $db
}


proc ProcessSources target {

	set db $agv::target($target)

	set sources [dict:at $db sources]
	set objects [dict:at $db objects]
	set rules [dict:at $db rules]
	set frameworks [dict:at $db frameworks]
	set type [dict:at $db type]

	set used_langs ""

	vlog "Performing general processing for $type '$target':"
	$::g_debug "SOURCES: $sources"

	set hdrs ""
	foreach s $sources {

		set info [pget agv::fileinfo($s)]
		set lang [dict:at $info language]
		set depspec [dict:at $db depspec]
		if {$depspec == ""} {
			set depspec [dict:at $agv::profile($lang) depspec]
		}
		#$::g_debug "DEPSPEC: $depspec"
		if { $depspec == "auto" } {
			# Check if you have depends declared explicitly. If so, use them.
			set info [pget agv::fileinfo($s)]
			if { [dict exists $info includes] } {
				$::g_debug " --- Headers for '$s' are known - not generating"
				set deps [concat $s [dict get $info includes]]
			} else {
				$::g_debug " --- Include info not found for '$s' - using gendep to generate:"
				set cflags [CompleteFlags $db $lang cflags]
				set deps [GenerateDepends $lang $cflags $s]
				# Write them back to the database
				dict set agv::fileinfo($s) includes [lrange $deps 1 end] ;# skip the source itself
				#parray agv::fileinfo
			}
		} else {
			# MAYBE, but so far not seen necessary
			#dict set agv::fileinfo($s) includes %$depspec
		}
	}

	set lang [dict:at $db language]

	# Write the database before running framework's hook
	set agv::target($target) $db

	ExecuteFrameworks $target compile-pre $sources

	# Re-read cache after possible changes were applied by the fw.
	set db $agv::target($target)

	# Moved here because the framework could have added something
	set target_deps [dict:at $db depends]

	foreach s $sources {

		set info [pget agv::fileinfo($s)]

		vlog "INFO($s): $info"

		# Now that $s is used ONLY AS FILENAME (NOT IDENTIFIER!)
		# it can be now localized towards build directory
		
		set s_abs [RealSourcePath $s]
		set s [prelocate $s_abs $agv::builddir]

		set lang [dict:at $info language]
		set depspec [dict:at $db depspec]
		if {$depspec == ""} {
			set depspec [dict:at $agv::profile($lang) depspec]
		}
		set depopt [dict:at $agv::profile($lang) depopt]

		set imgen [dict:at $db imgen]
		if { $imgen == "" } {
			set imgen [dict:at $agv::profile($lang) imgen]
		}
		if { $imgen == "" } {
			set imgen path
		}

		if { $type == "object" } {
			if { [llength $sources] != 1 } {
				error "Target '$target' is object and can have only one source file. Has: $sources"
			}
			set o [dict:at $db output]
			if { $o == "" } {
				error "Object target '$target' somehow has undefined 'output'!"
			}
		} else {
			# XXX This is required just to make things work, but it may be
			# required that this be optional, and "original directory preserved"
			# for o-files should be somehow supported.
			set o [GenFileBase $imgen $target $s].ag.o
		}

		$::g_debug " ... processing $s (language $lang) --> $o"

		# XXX The convention of dependency files may be generator dependent.
		# Currently the Makefile.tcl generator is used, so the dependency file
		# has *.ag.dep suffix and doesn't contain the *.o file spec. For Makefile
		# it should contain the *.o: statement. For other generators it may be
		# required to be even different. Also the structure is different. The
		# syntax for Makefile.tcl is: rule TARGET <DEPFILE { ... } and so it is
		# defined here. For Makefile it would have to be 'include(DEPFILE)' followed
		# by the build command line.
		if { $depspec == "cached" } {
			set depfile [GenFileBase $imgen $target $s].ag.dep

			# If there are no options, then add depfile generation rules.
			if { $depopt == "" } {

				# The separate-generation version. Used only when a compiler
				# does not support file generation simultaneously with the object file.
				$::g_debug " ... generating rule for dependency file $depfile @$agv::srcdir"
				if { ![dict exists $rules $depfile] } {
					set cflags [CompleteFlags $db $lang cflags]
					set depcmd [CreateDepGenCommand $lang $cflags [prelocate $s_abs $agv::srcdir]]
					set rule "$s {\n\t%gendep [prelocate $agv::srcdir $agv::builddir] $depfile $depcmd\n}"
					dict set rules $depfile $rule
				}
			}
			# Otherwise (if $depopt is nonempty), there will be a compile command
			# generated that generates the depfile in place (together with o-file).

			# Set "dependency list" as "please use deps saved in a file"
			# the info.include is unavailable in this mode.
			set deps <$depfile
		} else {

			$::g_debug "Dependency specification mode: $depspec"
			# This is for both "auto" and "explicit"
			# For "explicit" it just requires to be set primarily.
			set incs [dict get $info includes]
			set deps $s
			foreach i $incs {
				lappend deps [FixShadowPath $i]
			}
		}

		$::g_debug "DEPS: $deps"

		# Add rule to objects, notify in the database
		if { [lsearch $objects $o] == -1 } {
			lappend objects $o
		}

		if { ![info exists mkv::generated($o)] } {

			# Generate rule for the target
			set rule [GenerateCompileRule $db $lang $o $s $deps $target_deps]

			# Don't override rules that are set already.
			if { ![dict exists $rules $o] } {
				dict set rules $o $rule
			}

			set mkv::generated($o) [list $target $s $rule]
		} elseif { [lindex $mkv::generated($o) 0] == $target } {
			lassign $mkv::generated($o) target s rule
			vlog "NOTE: already generated rule for $target:$o from $s"
		} else {
			# Generate rule for the target
			set rule [GenerateCompileRule $db $lang $o $s $deps $target_deps]
			# Check if the rule is the same as the one already generated. If so, just pass on.
			# If not, report error.
			set oldrule [lindex $mkv::generated($o) 2]

			if { $rule != $oldrule } {
				puts stderr "+++ Error: Rule for [lindex $mkv::generated($o) 0]:$s would reuse $target:$s, but they differ:"
				puts stderr "+++ [lindex $mkv::generated($o) 0]: [string map {\n "; "} $oldrule]"
				puts stderr "+++ $target: [string map {\n "; "} $rule]"
				error "IM-file conflict. Please use -imgen with target-name or target-path"
			} else {
				puts stderr "+++ Note: Rule $s -> $o generated for both '$target' and '[lindex $mkv::generated($o) 0]'"
			}

			$::g_debug "NOT generating rule for '$o' - already generated for [lindex $mkv::generated($o) 0]:$s: $rule"
		}

		# Find headers among dependent files in the rules
		set ifiles [lrange $rule 1 end-1]

		set hsufs [agv::p::GetHeaderSuffixes $lang]

		# Find header files in the deps.
		# XXX It is not considered here that the list may consist
		# exclusively of a dep file, with < as first character. In this case
		# the dep file should be checked, and if possible the headers should
		# be extracted from there.

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
	set hdrs [pluniq $hdrs]
	foreach h $hdrs {
		if { $h ni $tnh && $h ni $th } {
			lappend tnh $h
		}
	}
	dict set db noinst-headers $tnh

	# Write back the database
	dict set db sources $sources
	dict set db objects $objects
	dict set db rules $rules

	set agv::target($target) $db

	ExecuteFrameworks $target compile-post $objects
}

proc Process:program target {

	set outfile [pget agv::target($target).output]
	if { $outfile == "" } {
		set outfile $target$agv::exe
		dict set agv::target($target) output $outfile
	}

	ProcessCompileLink program $target
	Process:phony $target
}

proc Process:library target {

	# NOTE: This immediate "archive" should be extracted
	# from "libspec" key.

	set libspec [pget agv::target($target).libspec]
	if { $libspec == "" } {
		set libspec static
	}

	set ipos 0

	set outfiles ""
	set output [pget agv::target($target).output]

	vlog "Processing library target=$target output=\[$output\] libspec=\[$libspec\]"

	foreach type $libspec {

		set debugad ""

		set outfile [lindex $output $ipos]
		set outfile_new [CreateLibraryFilename $target $type $outfile]

		if { $outfile != $outfile_new } {
			set debugad " (generated)"
			set outfile $outfile_new
		}

		vlog " ... For libspec=$type output=$outfile $debugad"
		lappend outfiles $outfile

		incr ipos

	}

	# Special procedure for Windows: on Windows a dynamic
	# and static library must not have the same rootname
	# because Windows compilers will generate the same filename
	# for a dll-importing library file and the static library
	# file (both LIBNAME.lib). This is to force user to use
	# an explicit name override for windows

	if { $::tcl_platform(platform) == "windows" } {
		# XXX Using this form of checking is actually wrong because
		# MinGW gcc, for example, doesn't suffer of this problem.
		# This is only to make the Microsoft compilers happy.

		# The term of "importer library" better be somehow explicitly
		# defined; it's a windows-specific thing not only not existing
		# in any other platform, but not even existing in other compilers
		# than Microsoft compilers. Nevertheless, the rule here is
		# that the library importer has the *.lib extension towards
		# the rootname of the shared library, which has *.dll extension.

		# Note that on Cygwin, the value of "platform" is unix, as
		# well as there's no problem there as well: the static library
		# has the ".a" suffix and the shared-library-importer library
		# has the ".dll.a" suffix (and for a library named 'name'
		# Cygwin makes 'cygname-VERSION.dll' and 'libname.dll.a' files).

		if { [llength $outfiles] > 1 && [lindex $outfile 0] in [lrange $outfile 1 end] } {
			error "On Microsoft platforms, if you need both static and shared library, they must have different names"
		}
	}

	if { $outfiles != "" } {
		dict set agv::target($target) output $outfiles
	}

	ProcessCompileLink library $target
	Process:phony $target
}

proc Process:object target {

	# Object will be the same thing as static library,
	# except that it will consist of one source file.
	# You can use this object file as dependency for
	# program or library.
	#
	# IMPORTANT: Normally, if a static library has
	# any kind of library-type dependency, it is
	# skipped and carried over to the latest target
	# program or shared library. In case when an
	# object target is a dependency, the object file
	# is added directly to the contents of the library.

	set outfile [pget agv::target($target).output]
	if { $outfile == "" } {
		if { [file extension $target] == "" } {
			set outfile ${target}.o
		} else {
			set outfile $target
		}
		dict set agv::target($target) output $outfile
	}

	ProcessCompileLink object $target
	Process:phony $target
}

proc Process:directory target {

	# The procedure must exist because there is a type
	# however it's hard to say as to whether anything
	# has to be done here.

	# XXX Well, there is one thing. The Silverfile that
	# is found for this directory (it is, right?) should
	# be loaded here - with changed context, of course -
	# so that the target types mentioned anywhere in the
	# dependencies in the current directory can be
	# properly picked up.
}

proc Process:custom target {
	# Custom targets are targets that declare the
	# input files (-s), output files (-o) and
	# the external command to build them.
	$::g_debug "PROCESSING CUSTOM: $target"

	set db $agv::target($target)

	set rules [dict:at $db rules]
	set phony [dict:at $db phony]
	set output [dict:at $db output]
	set outfile [ResolveOutput $output]
	set sources [dict:at $db sources]
	set command [dict:at $db command]
	set deps [dict:at $db depends]
	set usein [dict:at $db usein]

	foreach i {phony output outfile sources command deps} {
		$::g_debug "...D: $i: [set $i]"
	}

	# This will throw exception if any target in deps list
	# cannot be resolved
	set deptargets [ExtractActualDepends $target $deps]

	set rsrc ""
	foreach s $sources {
		lappend rsrc [FixShadowPath $s]
	}

	$::g_debug "DEP TARGETS: $deps"
	$::g_debug "DEP ACTUAL: $deptargets"
	$::g_debug "DEP SOURCES: $rsrc"

	set rule [list {*}$deptargets {*}$rsrc "\n\t$command\n"]
	foreach o $outfile {
		dict set rules $o $rule
	}

	if { $outfile != $target } {
		dict set phony $target $outfile
	}

	if { $outfile == "" } {
		# The case when we have a command-only-and-maybe-do-something
		# (so the above foreach loop did not pick up any files)
		dict set rules $target $rule
	}

	dict set db rules $rules
	dict set db phony $phony

	set subdir [pget db.installdir]
	set icat [pget db.install noinst]
	set prefix [dict:at $agv::profile(default) install:prefix]
	set icmds [GenerateInstallCommand $icat $outfile $prefix $subdir]

	set install_parts ""

	if {[IsDataUsage $target runtime]} {
		set installdeps [GetRuntimeDeps $target]
		dict set db runtimedeps $installdeps
		if {$icmds != ""} {
			dict set db rules install-$target-runtime [list $outfile {*}$installdeps \n$icmds\n]
		}
		dict set db phony install-$target-runtime ""
		lappend install_parts install-$target-runtime
	}

	if {[IsDataUsage $target devel]} {
		set installdeps [dict:at $db depends]
		if {$icmds != ""} {
			dict set db rules install-$target-devel [list $outfile {*}$installdeps \n$icmds\n]
		}
		dict set db phony install-$target-devel ""
		lappend install_parts install-$target-devel
	}

	if {$install_parts != ""} {
		dict set db phony install-$target {*}$install_parts
	}

	set agv::target($target) $db

	DebugDisplayDatabase agv::target $target

	Process:phony $target
}

# If -usein is not present, the default is runtime only.
# Otherwise it can be runtime or devel or both
proc IsDataUsage {target type} {
	set db $agv::target($target)
	set usein [dict:at $db usein]

	if {$usein == ""} {
		return [expr {$type == "runtime"}]
	}

	return [expr {$type in $usein}]
}

proc ReplaceVars s {
	return [regsub -all {%([A-Za-z0-9_-]+)} $s {${\1}}]
}

proc Process:pkg-config target {

	set db $agv::target($target)

	set name [dict:at $db name]
	set output [dict:at $db output]
	$::g_debug "PC: '$target' name='$name' output='$output'"

	# Usually the name of the target matches *.pc and it's
	# the name of the output file; if so, the name is then
	# the name with stripped .pc suffix. Otherwise this is
	# the name and the output is added .pc suffix. The second
	# one is not recommended because the name this way can
	# be mixed with some other target name, which is a usual
	# situation when you have a library with pkg-config.

	if { $name == "" && $output == "" } {
		# this is simplest, handle it accordingly.
		if { [string match *.pc $target] } {
			set output $target
			set name [file rootname $target]
		} else {
			set name $target
			set output $target.pc
		}

		dict set db name $name
		dict set db output $output

	} elseif { $name == "" } {
		set name $target
		if { [string match *.pc $target] } {
			if { $target != $output } {
				error "Target:$target has .pc in name and doesn't match output file '$output'"
			}
			set name [file rootname $target]
		}
	} elseif { $output == "" } {
		if { [string match *.pc $target] } {
			set output $target
		} else {
			set output $target.pc
		}
	}
	$::g_debug "PC FIXED: '$target' name='$name' output='$output'"

	set varset ""
	set keyset ""

	# First, complete the keys. Note that any %name must be replaced with ${name}.
	lappend keyset "Name: $name"
	lappend keyset "Description: [ReplaceVars [dict:at $db description]]" ;# Ignore if empty

	# Use version 0.1 if not defined.
	set ver [ReplaceVars [dict:at $db version]]
	if { $ver == "" } {
		set ver 0.1
	}
	lappend keyset "Version: $ver"
	set req [ReplaceVars [dict:at $db depends]]
	if { $req != "" } {
		set reqpublic ""
		set reqprivate ""
		foreach r $req {
			if { [string match private:* $r] } {
				lappend reqprivate [string range $r 8 end]
			} else {
				lappend reqpublic $r
			}
		}

		if { $reqpublic != "" } {
			lappend keyset "Requires: $reqpublic"
		}
		if { $reqprivate != "" } {
			lappend keyset "Requires.private: $reqprivate"
		}
	}

	set libs [ReplaceVars [dict:at $db ldflags]]
	if { $libs != "" } {
		set lpublic ""
		set lprivate ""
		foreach l $libs {
			if { [string match private:* $l] } {
				lappend lprivate "-l[string range $l 8 end]"
			} else {
				lappend lpublic "-l$l"
			}
		}

		lappend keyset "Libs: -L\${libdir} $lpublic"
		if { $lprivate != "" } {
			lappend keyset "Libs.private: $lprivate"
		}
	}

	set incdir [ReplaceVars [dict:at $db incdir]]
	set defines [ReplaceVars [dict:at $db defines]]
	set cflags [ReplaceVars [dict:at $db cflags]]

	set ocflags ""

	foreach d $incdir {
		if { [string index $d 0] != "/" } {
			# Relative path. For incdir, it requires prefixing with ${includedir}.
			append ocflags "-I\${incdir}/$d "
		} else {
			append ocflags "$d "
		}
	}

	foreach d $defines {
		append ocflags "-D$d "
	}

	lappend keyset "Cflags: -I\${includedir} $ocflags $cflags"

	# Ok, now variables
	# First, set variables that the brooker specified.
	foreach var [dict:at $db var] {
		set value [join [lassign [split $var =] varname] =]
		lappend varset $varname [ReplaceVars $value]
	}

	# Ok, varset is now built so that it can be accessed as a dictionary.
	# We need that to check if there are the overridable items specified.
	# If not, specify them. Use reverse order and insert at the beginning.
	PrependIfNotFound varset includedir {${prefix}/include}
	PrependIfNotFound varset libdir {${exec_prefix}/lib}
	PrependIfNotFound varset exec_prefix {${prefix}}
	PrependIfNotFound varset prefix [agv::prefix]

	# Good, all data are now complete. Open the file and write to it.
	set realtarget [file normalize [ResolveOutput1 $output b]]
	$::g_debug "Creating PC file: $realtarget"

	$::g_debug "PC VARIABLES: $varset"
	$::g_debug "PC KEYS: $keyset"

	set fd [open $realtarget w]

	# Now stream in the varset first
	foreach {key value} $varset {
		puts $fd "$key=$value"
	}

	# Var-key separator
	puts $fd ""

	# Keyset is a list of lines, so just print them as it goes
	foreach kv $keyset {
		puts $fd $kv
	}

	close $fd

	# Good. Now attach the installation rules
	set prefix [dict:at $agv::profile(default) install:prefix]
	set icmd [GenerateInstallCommand lib $output $prefix pkgconfig]

	dict set db rules install-$target [list $output "\n$icmd\n"]
	dict set db clean none

	dict set db rules $target [list "\n\techo 'The pc file should be already generated'\n"]
	dict set db flags {noclean distclean}

	set agv::target($target) $db
}

proc PrependIfNotFound {dictvar key value} {
	upvar $dictvar dict
	if { ![dict exists $dict $key] } {
		set dict [concat [list $key $value] $dict]
	}
}

proc Process:phony target {
	# Do the general depends processing
	set phony [dict:at $agv::target($target) phony]
	set deps [dict:at $agv::target($target) depends]

	# This sets the 'phony' key for a case when there's no
	# phony nor rule added for $target yet. Just in a case when
	# any earlier processing facility didn't set it at all.
	# Normally a processing facility should set a rule to build the target.
	set db $agv::target($target)
	if { [dict:at $db phony $target] == "" && [dict:at $db rules $target] == "" } {
		vlog "TARGET '$target' has no rule nor phony - setting phony with '$deps'"
		dict set agv::target($target) phony $target $deps
	}
}

proc GenerateInstallCommand {cat outfiles prefix {subdir ""}} {

	# It's hard to customize properly the command with the use
	# of multiple possible commands accepting various arguments.
	# Let's use a pattern that this is always "<command> <source> <target>".
	# The target directory must be precreated and for that there
	# is also a "<command> <directory-path>" name.

	set mkdir3cmd [dict:at $agv::profile(default) cmd:makedir]

	# XXX Consider generating here an internal Tcl command
	# that will only update the target if it's stale or missing.
	set installcmd [dict:at $agv::profile(default) cmd:install]

	# This is a "compiler platform target", not build target.
	set target [dict:at $agv::profile(default) target]

	set libsuffix ""
	if { [string match *64 [lindex [split $target -] 0]] } {
		set libsuffix 64
	}
	set havedir ""

	set icmd ""

	$::g_debug "GenerateInstallCommand cat=$cat outfiles={$outfiles} prefix=$prefix"

	lassign [split $cat :] catmain libspec
	switch -- $catmain {
		noinst - {} {
			# Nothing.
			set icmd "# Nothing to install for '$outfiles' (noinst)"
		}

		default {
			# Try to use the directory marked for particular category
			# NOTE: 'prefix' variable is here usually expanded
			set form [dict:at $agv::profile(default) installdir:$cat]
			# Fallback to form without extension, if not found
			if { $form == "" } {
				set form [dict:at $agv::profile(default) installdir:$catmain]
			}

			set instdir [subst $form]
			if { $instdir != "" } {
				if { $subdir != "" } {
					set instdir [file join $instdir $subdir]
				}
				set icmd "\t$mkdir3cmd $instdir"
				foreach outfile $outfiles {
					append icmd "\n\t$installcmd $outfile $instdir"
				}
			} else {
				puts stderr "+++AG WARNING: No installdir for -install $cat - can't install: $outfiles"
				set idirkeys [dict keys [dict filter $agv::profile(default) key installdir:*]]
				puts stderr "+++ Available: [pmap {k {string range $k 11 end}} $idirkeys]"
			}
		}
	}

	return $icmd
}

proc GenerateInstallTarget:object {target prefix} {
	return GenerateInstallTarget:library $target $prefix
}

proc GetFlatInstallDeps {target} {

	# Required is to:
	# 1. Get direct dependencies of the target
	# 2. For all those targets, extract names of the install targets
	# 3. Get runtime-dependent targets of this target and run itself on them
	# 4. Attach so extracted targets to the overall list and return.

	set all_install_deps ""

	foreach d [dict:at $agv::target($target) depends] {
		# The 'depends' field contains dependency spec together with libspec
		lassign [CheckDefinedTarget $d] d spec
		
		set phonies [dict keys [dict:at $agv::target($d) phony]]
		$::g_debug "Checking if '$d' is installable in phonies: $phonies"
		if { "install-$d" in $phonies } {
			lappend all_install_deps install-$d {*}[GetFlatInstallDeps $d]
		}
	}

	return $all_install_deps
}

# XXX This is kinda controversial - it should extract only runtime
# dependencies, so not devel, but then what is the static library
# doing there?
proc GetRuntimeDeps {target} {

	set db $agv::target($target)

	set runtype [dict get $db type]
	if { $runtype ni {program library custom phony} } {
		return
	}

	set deps [dict:at $db depends]
	if { $deps == "" } {
		# no deps - no runtime deps
		return
	}

	$::g_debug "GetRuntimeDeps: target '$target' deps: '$deps'"

	set deplist ""

	if { $runtype == "program" || ($runtype == "library" && "shared" in [dict:at $db libspec]) } {
		set runtype runtime
	}

	# Cases when this function is called are:
	switch -- $runtype {
		custom {
			# Check if the target has a specified -usein devel, if so
			# then it doesn't have runtime dependencies
			if { ![IsDataUsage $target runtime] } {
				return
			}

			# --> so find targets being data
			#    - targets of type 'library' mean error
			#    - targets of type 'program' are ignored (build dependency only)

			foreach d $deps {
				lassign [CheckDefinedTarget $d shared] d spec
				set ddb $agv::target($d)
				set t [dict get $ddb type]

				if {$t == "custom"} {
					if { [IsDataUsage $d runtime] } {
						lappend deplist $d
					}
				}

				if { $t == "library" && $spec == "shared" } {
					lappend deplist $d
				}

				# If dep target is any other, this is then a build dependency,
				# so "don't mention it".
			}

			return $deplist
		}

		runtime - phony {
			# - program or shared library:
			# --> so find all targets being shared library or data, only:
			#      - direct ones
			#      - transited through a static library

			foreach d $deps {
				lassign [CheckDefinedTarget $d shared] dd spec
				set ddb $agv::target($dd)
				set t [dict get $ddb type]
				$::g_debug "GetRuntimeDeps '$d' of '$target': $dd type=$t spec=$spec"

				set d $dd
				if { $t == "custom" } {
					# Add direct dependency.
					lappend deplist $d
				} elseif { $spec == "shared" } {
					# We don't even check if this was a "library".
					# There can be a "dymamic-oriented" dependency of a program,
					# which means that a program requires another program to function
					# (will be spawned by the dependent program).
					lappend deplist $d
				} elseif { $t == "library" } {
					# This catches only a static library then, as shared
					# is already handled.
					# Static libraries do carryover of their runtime deps.
					lappend deplist {*}[GetRuntimeDeps $d]
				}
				# Others are ignored.
			}

			return $deplist
		}

		default {
			return
		}
	}
}

proc GenerateInstallTarget:library {target prefix} {

	GenerateInstallDevel $target $prefix
	GenerateInstallRuntime $target $prefix install-$target-runtime

	dict set agv::target($target) phony install-$target [list install-$target-devel install-$target-runtime]
}

proc GenerateInstallDevel {target prefix} {
	set db $agv::target($target)

	# Outputs may be multiple and defined according to libspec.
	# We have to find libspec static, otherwise this output is
	# not to be generated
	set libspec [dict:at $db libspec]
	set ipos [lsearch $libspec static]
	if { $ipos != -1 } {
		set outfile [ppipe dict:at $db output | set output % |  lindex % $ipos]
		if { $outfile == "" } {
			error "IPE: no filename defined for static library target: $target (output:$output)"
		}
		set libfile [ResolveOutput $outfile]
		set libicmd [GenerateInstallCommand [dict:at $db install] $libfile $prefix]
	} else {
		set libicmd ""
	}

	set hcmds ""

	# Generate also installation for headers
	set src_hdr [dict:at $db headers]
	$::g_debug "... ($target): source headers to install: $src_hdr"
	set hdr [ResolveOutput $src_hdr s]

	$::g_debug "... ($target): target headers to install: $hdr"

	set hcmds [GenerateInstallCommand include $hdr $prefix [dict:at $db headers-installdir]]

	set itarget install-$target
	if { $libicmd != "" } {
		dict set db rules install-$target-archive [list $libfile \n$libicmd\n]
	}
	dict set db phony install-$target-archive ""

	dict set db rules install-$target-headers [list {*}$hdr \n$hcmds\n]
	dict set db phony install-$target-headers ""

	dict set db phony install-$target-devel [list install-$target-archive install-$target-headers]
	set agv::target($target) $db
}

proc GenerateInstallRuntime {target prefix itarget {type library}} {
	set db $agv::target($target)

	set needgen 0
	if {$type == "library"} {

		# Outputs may be multiple and defined according to libspec.
		# We have to find libspec shared, otherwise this output is
		# not to be generated
		set libspec [dict:at $db libspec]
		set ipos [lsearch $libspec shared]
		if {$ipos != -1} {
			set neeedgen 1
			set outfile [ppipe dict:at $db output | set output % | lindex % $ipos]
		}
	} elseif {$type == "program"} {
		# Always generate for type "program"
		set needgen 1
		set outfile [dict:at $db output]
	}
	if { $needgen } {
		if { $outfile == "" } {
			error "IPE: no filename defined for shared library target: $target (libspec:$libspec output:$output)"
		}
		set outfile [ResolveOutput $outfile]
		set icmd [GenerateInstallCommand [dict:at $db install]:shared $outfile $prefix]
		if { $icmd != "" } {
			set installdeps [GetRuntimeDeps $target]
			# As they are now available, record them for later use
			dict set db runtimedepends $installdeps

			dict set db rules $itarget [list $outfile {*}$installdeps \n$icmd\n]
		}
	} else {
		vlog "Target $target does not require shared library, no runtime install"
	}
	# If shared library was not requested, then just generate empty target.
	dict set db phony $itarget ""
	# Ok, ready. Write back to the database
	set agv::target($target) $db
}

proc GenerateInstallTarget:program {target prefix} {
	GenerateInstallRuntime $target $prefix install-$target program
}

proc ProcessCompileLink {type target} {

	# The ProcessLanguage does only language check for all sources
	# and defines the languages for every source file as well as for
	# the whole target. This is required prior to ProcessFlags, as
	# it will have to ask profile about prospective additional options.
	ProcessLanguage $target
	ProcessFlags $target
	ProcessSources $target

	# XXX BUG DEBUG - still a bug, remove after fixing
	set prev_outfile [pget agv::target($target).filename]
	set output [pget agv::target($target).output]
	$::g_debug " --**--++-- ProcessCompileLink: using output filenames '$output' (was $prev_outfile) for $type $target"

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

	$::g_debug "After-sources processed database for $target:"
	DebugDisplayDatabase agv::target $target

	# The rule for the outfile can be already generated; this
	# happens for "object" type where compile rule is the generation
	# rule itself.
	foreach outfile [dict:at $db output] libspec [dict:at $db libspec] {

		vlog "Generating rule for '$outfile' for libspec '$libspec'"

		if { [dict:at $rules $outfile] == "" } {
			# Libspec will be empty when not defined, but it still only
			# matters for a library anyway.
			set rule [GenerateLinkRule:$type $libspec $db $outfile]
			dict set rules $outfile $rule
		}

		# Add a phony rule that redirects the symbolic name to physical file,
		# in case they differ (in future, this can be due to them being in different directories)
		if { $outfile != $target && $outfile ni [dict:at $phony $target] } {
			vlog "Adding phony $target -> $outfile (because they differ)"
			dict lappend phony $target $outfile
		} else {
			vlog "NOT adding phony $target -> $outfile because $outfile is $target or among '[dict:at $phony $target]'"
		}
	}

	# Ok, ready. Write back to the database
	dict set db rules $rules
	dict set db phony $phony
	set agv::target($target) $db

	#$::g_debug "DATABASE: agv::profile\[default\]"
	#DebugDisplayDatabase agv::profile default

	set prefix [dict:at $agv::profile(default) install:prefix]
	if { $prefix == "" } {
		puts stderr "+++ AG WARNING: 'install:prefix' not found in profile - not generating install targets"
	} else {
		$::g_debug "Generating install rules for '$target' as '$type' in '$prefix'"
		GenerateInstallTarget:$type $target $prefix
	}

	ExecuteFrameworks $target complete
}

proc CompleteFlags {db lang flagtype} {
	# General $flagtype for the target
	set flags [dict:at $db $flagtype]

	# General language $flagtype applicable for all targets
	# For example, definitions will be collected here for -D option
	# The value of agv::profile($lang).$flagtype will be also included
	set lang_flags [GetUnifiedProfileFlags $lang $flagtype]

	$::g_debug " ... target's $flagtype: $flags"
	$::g_debug " ... lang($lang) $flagtype: $lang_flags"

	append flags " $lang_flags"

	return $flags
}

proc GenerateCompileRule {db lang objfile source deps target_deps} {

	if { ![info exists agv::profile($lang)] } {
		error "Silvercat doesn't know how to compile '$lang' files.\nPlease select correct profile"
	}

	$::g_debug "Generating compile rule for '$objfile' from '$source':"

	set cflags [CompleteFlags $db $lang cflags]

	# The rule should be formed as for make.tcl
	# except the 'rule' command and the target name.
	# So: source-deps... command-to-compile

	set compiler [dict get $agv::profile($lang) compile]
	set oflag [dict get $agv::profile($lang) compile_oflag]

	if {$target_deps != ""} {
		append target_deps " "
	}

	set depflags ""
	set depopt [dict:at $agv::profile($lang) depopt]
	if { [string index $deps 0] == "<" && $depopt != "" } {
		# Rule for deps wasn't generated then, although mentioned here.
		# It's predicted to generate depfile and object file in one step
		set depfile [string range $deps 1 end]
		set depflags "${depopt}$depfile"
		set deps "$source $target_deps$deps"

		$::g_debug "ONESTEP dep for '$source': $deps ($source from $agv::srcdir relative to $agv::builddir)"
	} else {
		set deps "$target_deps$deps"
	}

	# Ok, now we need to readjust source and deps to be in
	# the srcdir
	$::g_debug "GENERATING FOR  $source: $deps"

	set command "$compiler $cflags $depflags $source $oflag $objfile"
	set maybe_dump ""
	# This -dumpinfo flag is propagated from profile, so check both
	# this target and profile.
	# XXX Some more general method must be developed for this so that there
	# can be also "config" objects that can apply the settings to only selected targets
	set di [psfirst [dict:at $db dumpinfo] [dict:at $agv::profile($lang) dumpinfo]]

	# Some other possible values should be available in future. Now handle only yes/on/true
	if { $di != "" && [string is true $di] } {
		set infofile [file rootname $objfile].info
		set maybe_dump "\n\t@%pwrite $infofile {[GenerateInfoGen $source $objfile $command]}"
	}

	$::g_debug "... Command: $command"

	set rule "$deps {\n\t$command$maybe_dump\n}"

	$::g_debug "... Generated rule: $rule"

	return $rule
}

proc GenerateInfoGen {source target command} {
	return "Source {$source} Target {$target} Command {$command}"
}

proc FindDepSpec {target reqspec} {
	# Extract filename according to the specification.
	set libspec [dict:at $agv::target($target) libspec]
	if { $libspec == "" } {
		set libspec static
	}

	if { [llength $libspec] < 2 } {
		if { $reqspec != $libspec } {
			return -1
		}
		return 00 ;# Double 0 to state that the value shouldn't be indexed, but taken as a whole
	} else {
		return [lsearch $libspec $reqspec]
	}
}

proc GetTargetFile {target reqspec} {

	set output [dict:at $agv::target($target) output]

	if { $reqspec != "" } {
		set pos [FindDepSpec $target $reqspec]
		if { $pos == -1 } {
			return
		}
		if { $pos ne "00" } {
			# 00 returned means that the output is already set correctly
			# (because the $target has only one reqspec, and it matches $reqspec)
			set output [lindex $output $pos]
		}
		set reqspec [lindex $reqspec $pos]
	}

	set outs ""

	foreach o $output {
		set filename [ResolveOutput1 $o]
		if { $filename == "" } {
			$::g_debug "ERROR: no filename set in this database:"
			DebugDisplayDatabase agv::target $target
			error "Output file not defined for $target"
		}

		lappend outs $filename
	}

	return [list $outs $reqspec]
	#set dir [file dirname $target]
	#return [file join $dir $filename]
}

## This function resolves the declared output file into the
## expected relative path to be put into Makefile.
# The path is expected to be relative and the default base
# path for this file is the source path, as per @a defaultprefix,
# or this path can explicitly specify that it should be in the
# normalized directory: // or //s: for source, //b: for build,
# //t: for toplevel, or any custom //PREFIX: if agv::PREFIXdir
# variable is defined.
#
# WINDOWS NOTES: Drive based path c:/sources/file.cc will work
# normally because it's not //-based. However UNC paths won't work,
# you should mount the path to a local drive to be able to use it.
proc ResolveOutput1 {o {defaultprefix b}} {

	$::g_debug "ResolveOutput1: $o (default: $defaultprefix)"

	# The output path is expected to be a relative path
	# to builddir, unless it's a path outside the toplevel,
	# in which case it should be absolute.

	# The path should be:
	# - without special prefix, then it's treated as relative
	#   to $defaultprefix (as if it had //$defaultprefix: prefix)
	# - with //, //s:, //b:, //t: they are treated as residing there,
	#   so the path should be relocated to be relative to build dir.

	set initial [string range $o 0 1]

	if { $initial != "//" } {

		if { [file pathtype $o] == "absolute" } {
			$::g_debug "... ABSOLUTE PATH. Relocating to '$agv::builddir' up to '$agv::toplevel'"
			return [prelocate $o $agv::builddir $agv::toplevel]
		}

		# If this is a builddir, with prefix=builddir
		# expected relative to builddir, then simply
		# return it as is.
		if { $defaultprefix == "b" } {
			$::g_debug "... NO PREFIX, returning unchanged: $o"
			return $o
		}

		# Otherwise apply the prefix artificially and continue processing
		set o //${defaultprefix}:$o
		$::g_debug "... required s, adding prefix: $o"
	}

	set rest [string range $o 2 end]

	set parts [file split $rest]
	#puts "PARTS: $parts"
	set first [lindex $parts 0]
	lassign [split $first :] prefix part0
	#puts "FIRST: '$first' PREFIX: '$prefix' PART0: '$part0'"

	if { $prefix == $first } {
		# No : found - treat it as part of the path
		set part0 $prefix
		set prefix s
	} elseif { $part0 == "" } {
		# We have //x:/rest/of/path
		set part0 [lindex $parts 1]
		set parts [lrange $parts 1 end]
	}

	set restpath [file join $part0 {*}[lrange $parts 1 end]]

	set apath ""

	switch -- [string index $prefix 0] {
		s {
			set apath $agv::srcdir
		}

		b {
			set apath $agv::builddir
		}

		t {
			set apath $agv::toplevel
		}

		default {
			set var "agv::${prefix}dir"
			if { [info exists $var] } {
				set apath [set $var]
			} else {
				error "Prefix '//${prefix}: requires agv::${prefix}dir variable to be defined."
			}
		}
	}

	$::g_debug "... RESOLVE: apath=$apath restpath=$restpath prefix=$prefix builddir=$agv::builddir"

	if { $apath == "" } {
		error "Invalid special-path specification: $prefix (in $o) - use '//SPEC:', where SPEC is s, b, t"
	}

	set relpath [prelocate [file join $apath $restpath] $agv::builddir]

	return $relpath
}

proc ResolveOutput {output {f b}} {
	set out ""
	foreach o $output {
		lappend out [ResolveOutput1 $o $f]
	}
	return $out
}

proc GetDependentLibraryTargets {target {transit transit}} {
	set depends [dict:at $agv::target($target) depends]  
	$::g_debug "Getting dependent targets for '$target': $depends"
	set libpacks ""
	set langs ""

	lassign [CheckDefinedTarget $target] t tspec

	# Deps should contain more-less the same as $depends, but
	# when there's a dependency in a form of a static library,
	# this should add the library file directly (instead of the
	# target) together with all this library's dependent libraries
	# and -l flags.

	set deps ""

	foreach d $depends {

		$::g_debug "Getting DEP: '$d'"
		lassign [CheckDefinedTarget $d] d spec
		if { $d == "" } {
			error "Target '$d' (dependency of $target) is not defined"
		}
		$::g_debug "CheckDefinedTarget: DEP=$d SPEC=$spec"

		set type [dict:at $agv::target($d) type]

		# Check if this is an imported target
		if { [info exists agv::foreign($d)] } {
			set type [dict:at $agv::foreign($d) type]
			vlog "--- taking foreign target '$d' type '$type'"
		}

		# XXX PROBLEM TO SOLVE:
		# This procedure should not return DIRECT DEPENDENT LIBRARY FILE, only the extra dependencies.
		# This procedure should extract the exact dependent library file on its own here, the deeper call
		# should only return extra library packs, NOT ITSELF.

		if { $type == "library" } {
			set slibs ""
			lassign [GetTargetFile $d $spec] libofile ospec
			if { $libofile == "" } {
				error "Target '$target' requested dep '$d' in spec '$spec' which '$d' does not define"
			}
			$::g_debug " -- LIBRARY DEP '$d': output=[pget agv::target($d).output] ($libofile) SPEC: $spec"
			set ldflags ""
			if { $spec == "static" } {
				# Static libraries must carry over all its ldflags to the target
				set slibs [dict:at $agv::target($d) ldflags]
				$::g_debug " -- static library $d - importing its ldflags: $slibs"
			} else {
				$::g_debug " -- shared library $d - not importing ldflags"
			}

			set libpack [list $libofile $slibs]

			set langs [dict:at $agv::target($d) language]
			$::g_debug " --- Target '$d' provides libraries: $libpack (language: $langs)"

			# Recursive call
			# XXX consider unwinding - recursion in Tcl is limited and may
			# result in internal error!
			if { $transit == "transit" || ( $spec == "static" && $tspec == "static") } {
				$::g_debug "--> DESCENDING INTO '$d' to get transitive dependencies {"
				lassign [GetDependentLibraryTargets $d [pif {$spec == {static}} ? transit : notransit]] adlibpacks adlangs addeps
				$::g_debug "} LIBPACKS from dep '$d': $adlibpacks; DEPS: $addeps, adding if spec($spec) is static"
				if { $spec == "static" } {
					lappend deps {*}$addeps
				}
				# Ok, according to the libpack rule, REQUESTER PROVIDER order should be kept.
				# So, the dependent libpacks land just after this one.
				lappend libpacks {*}[concat [list $libpack] $adlibpacks ]
				lappend langs {*}$adlangs
			} else {
				$::g_debug "--| NOT DESCENDING into '$d' transit=$transit spec=$spec parent-spec=$tspec"
			}

			if { $spec == "static" } {
				$::g_debug "ADDING DEP: $libofile (because static library)"
				lappend deps $libofile

			} else {
				$::g_debug "ADDING DEP: $d (because shared library)"
				lappend deps $d
			}

		} elseif { $type == "object"} {
			lappend libpacks [dict get $agv::target($d) output]
			# XXX Object file may also have dependencies!
			$::g_debug "ADDING DEP: $d (because object file)"
			lappend deps $d
		} else {
			$::g_debug "ADDING DEP: $d - Target of type '$type' does not provide dependent transitive libraries."
			lappend deps $d

		}
	}

	$::g_debug " --- RESULTING DEP LDFLAGS FOR '$target': $libpacks"

	return [list $libpacks $langs $deps]
}

proc IntegrateLibraryPacks {libpacks} {
	# This expects libpacks in the following form:
	# {library-files library-flags}...
	# If these libraries mentioned in the library-files have
	# dependencies, they must be satisfied in packs following
	# this one.
	#
	# Now the role of this function is to make them all unique
	# and in order, provided that REQUESTER PROVIDER order must
	# be preserved always for static libraries.
	#
	# In particular it means that you should review the packs
	# from the last to the first, and if in the next reviewed
	# pack something repeats, it must be ignored. This concerns
	# separately flags and separately library files

	set libfiles ""
	set libflags ""

	$::g_debug "LIBPACKS:\n$libpacks"
	set revpacks [lreverse $libpacks]
	$::g_debug "REV-LIBPACKS:\n$revpacks"

	foreach pack [lreverse $libpacks] {
		lassign $pack file flags
		$::g_debug "IntegrateLibraryPacks: reviewing pack: file=$file flags=$flags"
		set efflags ""
		foreach f $flags {
			if { $f ni $libflags } {
				lappend efflags $f
				$::g_debug " ... adding flag '$f'"
			} else {
				$::g_debug " ... NOT adding flag '$f' (already present)"
			}
		}

		set libflags [concat $efflags $libflags]

		if { $file ni $libfiles } {
			set libfiles [concat $file $libfiles]
			$::g_debug " ... adding file $file"
		} else {
			$::g_debug " ... NOT adding file $file - already present"
		}
	}

	return [concat $libfiles $libflags]
}

proc GenerateExecutableLinkRule {type db outfile} {

	# Check dependent targets. If this target has any dependent targets of type library,
	# add its library specification to the flags.
	lassign [GetDependentLibraryTargets [dict:at $db name]] libpacks langs depends
	lappend langs {*}[dict:at $db language]
	set langs [lsort -unique $langs]
	set lang [agv::p::GetCommonLanguage $langs]
	vlog "GLR/p: Common language for '$langs': $lang"

	set objects [dict:at $db objects]
	set ldflags [CompleteFlags $db $lang ldflags]

	set libs [IntegrateLibraryPacks $libpacks]

	if { $type == "library" } {
		set linker [pget agv::profile($lang).linkdl]
		if { $linker == "" } {
			# Check if you can glue link and dlflags
			set linker "[pget agv::profile($lang).link] [pget agv::profile($lang).dlflags]"
			# We don't care if it is valid or not.
		}
	} else {
		set linker [dict get $agv::profile($lang) link]
	}

	set oflag [dict get $agv::profile($lang) link_oflag]

	$::g_debug "Generating link rule for '$outfile' ldflags: $ldflags libs: $libs"

	set command "$linker $objects $oflag $outfile $libs $ldflags"

	set maybe_dump ""
	set di [psfirst [dict:at $db dumpinfo] [dict:at $agv::profile($lang) dumpinfo]]
	if { $di != "" && [string is true $di] } {
		set infofile $outfile.ag.info
		set maybe_dump "\n\t%pwrite $infofile {[GenerateInfoGen $objects $outfile $command]}"
	}
	# The rule should contain all ingredient files
	# and all "targets" declared here as its dependency
	# (be it phony or file-based target). The exact file that
	# needs to be used in the command is already extracted
	# by GetDependentLibraryTargets.
	set rule "$objects $depends {\n\t$command$maybe_dump\n}"

	return $rule
}

proc GenerateLinkRule:program {subtype db outfile} {
	return [GenerateExecutableLinkRule program $db $outfile]
}

# NOTE: static libraries are just archives with *.o files, they don't
# have dependencies - at worst they will be resolved at link time with
# shared libraries or executables.
proc GenerateLinkRule:library {libtype db outfile} {

	if { $libtype == "shared" } {
		return [GenerateExecutableLinkRule library $db $outfile]
	}

	# The 'libspec' key can contain 'static' and 'shared'.
	# If there's none, it defaults to 'static'.
	# If both are supplied, create link rules for both!

	set lang [pget db.language]
	set arcmd [dict:at $agv::profile($lang) archive]
	if { $arcmd == "" } {
		#parray agv::profile
		error "Can't create rules for static library [pget db.name] lang=$lang - profile key 'archive' returns no commands"
	}

	set objects [dict:at $db objects]
	set deps [dict:at $db depends]

	# Add all dependent objects to the archive (NOT dependent libraries)
	foreach d $deps {
		lassign [CheckDefinedTarget $d] d spec
		if { [dict:at $agv::target($d) type] == "object" } {
			lappend objects [dict get $agv::target($d) output]
		}
	}
	set command "$arcmd $outfile $objects"

	set maybe_dump ""
	set di [psfirst [dict:at $db dumpinfo] [dict:at $agv::profile($lang) dumpinfo]]
	if { $di != "" && [string is true $di] } {
		set infofile $outfile.ag.info
		set maybe_dump "\n\t%pwrite $infofile {[GenerateInfoGen $objects $outfile $command]}"
	}

	set rule "$objects {\n\t$command$maybe_dump\n}"
	return $rule
}

proc GenerateLinkRule:object {libtype db outfile} {

	error "GenerateLinkRule [dict:at $db name]: for object the rule should be already defined as compile rule"
}

proc silvercat-recog-line {} { return "# Generated by Silvercat" }

proc ag-do-genrules target {

	# Default target is ".".
	# This is not as a "standard for makefiles" that it is
	# normal to define "all" as first target. The "all"
	# target is a special name for silvercat and it is
	# always available as a target having everything else
	# as dependency.
	#
	# Note that this rule was changed a bit since intoducing
	# the on-demand targets. So, "all" includes all targets
	# except those on-demand, whereas the target that includes
	# everything is "." - and this target also isn't written
	# into Makefile. This includes only symbolically all
	# targets for the needs of the internal processing.
	if { $target == "" } {
		set target .
	}
	set makefile [file join $mkv::directory Makefile.tcl]

	# Check existing makefile
	if { [file exists $makefile] } {
		set fd [open $makefile r]
		set firstline [string trim [gets $fd]]
		if { $firstline != [silvercat-recog-line] } {
			puts stderr "+++ ERROR: The $makefile file to be generated exists"
			puts stderr "+++ and it's not a Silvercat-generated file. If this '$makefile'"
			puts stderr "+++ is something you don't need, please delete it first."
			return 1
		}
	}

	set agfile_inmake [prelocate $::agfile $agv::builddir]
	set variables [pget ::g_variables]
	set varexpr ""
	foreach {vname vval} $variables {
		append varexpr "'$vname=$vval' "
	}

	set cmdf \
{
	[mkv::MAKE] clean && !agcmd -f !agfile !varexpr !options
}

	# These actions were added previously to enforce restarting make and do nothing afterwards.
	# This is now supported by make as autodetected makefile regeneration, so it's no longer needed
	#%submake [pget mkv::targets]
	#%exit

	# For genrules, add also reconfigure rule to regenerate Makefile.tcl.
	# The 'reconfigure-ifneeded' is running automatically to check if Makefile.tcl is fresh.
	# The 'reconfigure' target can be only run on demand.
	set maybe_options ""
	if { $agv::toplevel != "" } {
		set top [prelocate $agv::toplevel]
		if { $top != "." } {
			lappend maybe_options -t $top
		}
	}
	
	if { $agv::p::profile_overrides != "" } {
		foreach {k v} $agv::p::profile_overrides {
			lappend maybe_options -p $k $v
		}
	}

	# XXX config_overrides (-c) is also required, add when implemented

	ag reconfigure -type custom -flags noclean distclean -clean none -runon demand \
			-command {[string map [list !agcmd [agv::AG] !agfile $agfile_inmake !varexpr $varexpr !options $maybe_options] $cmdf]}

	ag reconfigure-ifneeded -type custom -flags noclean distclean -clean none -sources $::agfile -output Makefile.tcl $agv::generated_files \
			-command {	%submake reconfigure}

	vlog "([pwd]) ALL DEFINED TARGETS:\n\t[array names agv::target]"
		
	# Complete lacking values that have to be generated.
	if { ![agp-prepare-database $target] } {
		puts stderr "+++ ERROR: Failed to prepare database for '$target'"
		return 1
	}

	vlog "Database for '$target' completed. Generating Makefile"

	set fd [open $makefile w]

	# Print header
	# Just don't print this in the sub-calls
	puts $fd [silvercat-recog-line]
	puts $fd "# DO NOT MODIFY THIS FILE - or remove the above comment line."
	puts $fd "# This file is generated and its contents will be overwritten.\n"

	set ok [GenerateMakefile $target $fd]
	if { !$ok } {
		puts $fd "error {XXX THIS MAKEFILE IS INVALID. Please check errors and regenerate}"
	}

	close $fd
	return [expr {$ok ? 0 : 1}]
}

proc GenerateMakefile {target fd} {

	if { $target in $agv::genrules_done } {
		vlog "NOT Generating rules for $target -- already generated"
		return
	}

	puts $fd "# GENERATE FOR TARGET: $target"

	vlog "Generating makefile for $target"

	# Put exported variables and procedures into the script
	foreach vv $agv::p::exported_var {
		lassign $vv v a
		puts $fd "set $v {$a}"
	}
	foreach vv $agv::p::exported_proc {
		lassign $vv n a b
		puts $fd "proc $n {$a} {$b}" 
	}
	# Clear them so that they don't get generated again
	set agv::p::exported_var ""
	set agv::p::exported_proc ""

	if { $target == "." } {
		$::g_debug "NOTE: not generating any rules for toplevel target"
	} else {

		# Normal targets, including 'all'.
		$::g_debug "Generating rules for '$target'"

		set rules [dict:at $agv::target($target) rules]
		set type [dict:at $agv::target($target) type]
		set flags [dict:at $agv::target($target) flags]
		set phony [dict:at $agv::target($target) phony]

		# Rules is itself also a dictionary.
		# Key is target file, value is dependencies and command at the end.

		if { $rules != "" } {
			foreach {tarfile data} $rules {
				puts $fd "rule $tarfile $data"
				if { $flags != "" } {
					puts $fd "setflags $tarfile $flags"
				}
			}
		} else {
			# Apply flags to the target, if any, for the target name
			if { $flags != "" } {
				puts $fd "# Not defining rules for '$target' (type: $type) - none generated."
				puts $fd "setflags $target $flags"
			}
		}

		# First rules, then phony. Later phonies may override earlier rules.

		if { $phony != "" } {

			# For 'all' target we need that the 'all' phony rule
			# is firstmost. There's already ensured that 'all' is
			# always the first dependency in the dependency list of . target.
			# We know also that it has 'all' phony rule that includes all deps.
			# So, just shift this rule to the first position.
			set alldone false
			if { $target == "all" } {
				set alldone true

				puts $fd "# EXPOSING all first"
				set all_deps [dict:at $phony all]
				puts $fd "phony all $all_deps"
				set all_install_deps ""

				$::g_debug "Generating for 'all' and its deps: $all_deps"

				# On the first level, just extract all DIRECT dependencies
				# from 'all' itself. Then, from any of these extract the
				# direct runtime dependencies.
				foreach d $all_deps {

					# XXX
					# This must be tested more thoroughly.

					$::g_debug "ALL: Looking for targets in dep '$d'"
					set this_phony [dict:at $agv::target($d) phony]
					if { "install-$d" in $this_phony } {
						lappend all_install_deps install-$d
						puts $fd "# -- $d (direct): install-$d"
						$::g_debug "... Found 'install-$d' in agv.target.$d.phony ($this_phony), generating install target"
					} else {
						$::g_debug "... No 'install-$d' found in agv.target.$d.phony (found: [dict keys $this_phony])"
					}
					set ddeps [GetFlatInstallDeps $d]
					puts $fd "# -- $d: $ddeps"
					lappend all_install_deps {*}$ddeps
				}
				set all_install_deps [pluniq $all_install_deps]
				puts $fd "phony install $all_install_deps"
			}

			foreach {rule deps} $phony {
				if { $alldone && $rule == "all" } {
					continue
				}
				$::g_debug "Generating for target '$target' - forwarder to deps: $deps"
				puts $fd "# TO GENERATE phony rule '$rule'"
				puts $fd "phony $rule $deps"
			}
		} else {
			$::g_debug "Phony empty for target '$target', not generating installation"
		}

		puts $fd ""
	}

	lappend agv::genrules_done $target

	# Now generate everything for the dependent targets
	set deps [dict:at $agv::target($target) depends]

	vlog "Makefile generator: will generate sub-rules for deps: $deps"
	foreach d $deps {
		lassign [CheckDefinedTarget $d] d spec
		#if { [IsSubTarget $d] } {
		#	vlog " --- skipping target in a subdir: $d"
		#	continue
		#}
		puts $fd "# TO GENERATE '$d' dependency of '$target'"
		GenerateMakefile $d $fd
	}
	vlog "Makefile generator: finished sub-rules for deps"

	puts $fd [SynthesizeClean $target]

	return true
}

proc SynthesizeClean {target} {
	set type [dict:at $agv::target($target) type]
	set customclean [dict:at $agv::target($target) clean]
	if { [string trim $customclean] == "none" } {
		vlog "Synthesizing clean: custom clean rule says 'none' - not generating any clean for '$target'"
		return "# Not synthesizing clean rule for '$target' - unwanted."
	}

	set parts [file split $target]
	# use [join <parts> /] not [file join] because the latter
	# doesn't work with empty arguments!
	set dir [join [lrange $parts 0 end-1] /]
	set tarname [lindex $parts end]

	vlog "Makefile: checking for clean for '$tarname' in $dir ($type)"

	if { $tarname == "." } {
		set cleanname clean
	} else {
		set cleanname $tarname-clean
	}

	if { $tarname ni {all .} && $type ni {program library custom} } {
		return "# Not synthesizing clean target for $type $target"
	} 

	# Ok, now the generation in a subdirectory should
	# forward it to the original makefile.
	if { $dir != "" } {
		vlog "Makefile generation: synthesizing '$dir/$cleanname' by forwarding to $dir"

		# Add it also to the list of clean targets for that target.
		# It's needed to spit it out as an overall clean target dependency.
		lappend agv::target($target) cleantarget $dir/$cleanname

		return "rule $dir/$cleanname {\n\t%submake -C $dir $cleanname\n}\nphony $dir/$cleanname"
	}

	set cleandeps ""
	set cleanflags ""
	if { $tarname in {all .} } {
		#append orule "# Gen clean for '$tarname' -- deps are: [ag $tarname ?depends]\n"
		# This is a synthetic 'all' target, so take clean names from
		# all dependent targets, if any. They must be dependencies of clean.
		foreach d [ag $tarname ?depends] {
			lassign [CheckDefinedTarget $d] d spec
			lappend cleandeps {*}[dict:at $agv::target($d) cleantarget]
		}
		set cleanflags "-noclean"
	}
	if { $tarname == "." } {
		lappend cleandeps all-clean
	}

	vlog "Makefile generating: synthesizing '$cleanname' target to clean '$target' (with extra $cleandeps)"
	if { $customclean != "" } {
		vlog "... HAVE CUSTOM CLEAN: {\n$customclean}"
		set cleancmd $customclean
	} elseif { $tarname == "." } {

		# The . target will not generate anything in makefile, so %autoclean . will fail.
		# You have to drive autoclean from every dependent target
		# XXX Check it this doesn't have to be a more general rule and instead
		# this should't be checked whether the target has a chance to get generated.
		set cleancmd "\n"
		foreach d [ag . ?depends] {
			lassign [CheckDefinedTarget $d] d spec
			append cleancmd "\t%autoclean $d $cleanflags\n"
		}
	} else {
		set cleancmd "\n\t%autoclean $target $cleanflags\n"
	}
	append orule "rule $cleanname $cleandeps {$cleancmd}\nphony $cleanname"
	if { $tarname == "all" } {
		append orule "\nrule all-distclean all-clean \{\n"
		foreach d [pget agv::p::directories] {
			append orule "\t%submake -C $d distclean\n"
		}
		append orule "\t%autoclean $target\n\t%autoclean $target distclean\n\}"
	} elseif { $tarname == "." } {
		append orule "\nrule distclean clean all-distclean \{\n"
		foreach d [pget agv::p::directories] {
			append orule "\t%submake -C $d distclean\n"
		}
		foreach d [ag . ?depends] {
			lassign [CheckDefinedTarget $d] d spec
			append orule "\t%autoclean $d\n\t%autoclean $d distclean\n"
		}
		foreach f $agv::generated_files {
			# XXX The 'rm' command should be system dependent!
			append orule "\trm -f $f\n"
		}
		append orule "\}"
	}

	return $orule
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

	# Now the make script is no longer fully sourced into ag.tcl.
	# If you want to run make, it's better done in a separate interpreter.

	interp create ag-interp-make

	ag-interp-make eval set argv0 $mkv::p::gg_makepath

	# Fake interactive mode so that make.tcl is used as library
	# (the [main] command isn't executed)
	ag-interp-make eval set tcl_interactive 1
	
	# Source-in the make commands
	ag-interp-make eval source $mkv::p::gg_makepath

	#ag-interp-make eval set mkv::debug puts

	set rules [dict:at $agv::target($target) rules]

	# Now define the rules according to the 
	set phony [dict:at $agv::target($target) phony]

	if { $phony != "" } {
		foreach {rule deps} $phony {
			vlog "Preparing phony: ($rule) ($deps)"
			ag-interp-make eval phony $rule {*}$deps
		}
	}

	set rules [dict:at $agv::target($target) rules]

	# Rules is itself also a dictionary.
	# Key is target file, value is dependencies and command at the end.

	foreach {tarfile data} $rules {
		vlog "Preparing rule: ($tarfile) ([lrange $data 0 end-1]) ([lindex $data end])"
		ag-interp-make eval [concat rule $tarfile $data]
	}

	ag-interp-make eval [concat rule $target-clean "
	%autoclean $target
	"]

	ag-interp-make eval make {*}$exp_targets

	interp delete ag-interp-make

	return 0
}

proc ag-make {target} {
	set wd [pwd]
	file mkdir $agv::builddir
	cd $agv::builddir
	ag-do-make $target
	cd $wd
}

proc ExtractActualDepends {target deps} {
	# Filter deptargets through the library specialization as requested
	# By the way this also checks if the dep target is defined
	set tartype [dict:at $agv::target($target) type]
	set deptargets ""
	foreach d $deps {
		lassign [CheckDefinedTarget $d] d spec
		if { $d == "" } {
			error "Target '$d' (dependency of '$target' type $tartype) is not defined"
		}
		if { $spec == "" } {
			lappend deptargets $d
			continue
		}

		# For a library specification, check if such a specification is available.
		set libspec [dict:at $agv::target($d) libspec]
		if { $libspec == "" } {
			error "Target '$d' spec $spec (dependency of '$target' type $tartype) has no libspec defined"
		}

		# Find specialization that is requested
		set ix [FindDepSpec $d $spec]
		if { $ix == -1 } {
			error "Target '$d' has no spec $spec (required as dependency of '$target type $tartype)"
		}

		# Store a library file as a dependency.
		set depfile [ppipe dict:at $agv::target($d) output | set output % | lindex % $ix]
		if { $depfile == "" } {
			error "IPE: In target '$d' output file not found for spec=$spec (libspec:$libspec ouptut:$output)"
		}
		lappend deptargets $depfile
	}

	return $deptargets
}

# This procedure should be used instead of checking if exists agv::target($target).
# It's specifically for targets that may have a special form and because of that
# have to be synthesized lazily.
# Currently it concerns only detecting "directory-based targets" and libspec.
proc CheckDefinedTarget {target {dspec {}}} {

	# If the target exists - it's already done.
	if { [info exists agv::target($target)] } {
		# It this was a library, then return the first default
		if { [dict:at $agv::target($target) type] == "library" } {
			set ls [dict:at $agv::target($target) libspec]
			if { $dspec == "" } {
				set spec [lindex $ls 0]
				if { $spec == "" } {
					set spec static
				}
				return [list $target $spec]
			} else {
				if { ($dspec == "static" && $ls == "") || $dspec in $ls } {
					return [list $target $dspec]
				} else {
					return $target
				}
			}
		}
		return $target
	}

	# Maybe it's undefined because it's directory based.
	set parts [file split $target]
	if { [llength $parts] < 2 } {
		# Not a directory-based target, so it must be explicitly defined
		return
	}

	# It is directory based.
	# So, check if directory target is defined
	set dir [lindex $parts 0]
	if { ![info exists agv::target($dir)] } {
		# XXX Maybe do lazy-define of the target?
		error "Target '$target' is parent/child form, but no target '$dir' is defined (use ag-subdir to define)"
	}

	set type [dict:at $agv::target($dir) type]

	switch -- $type {
		default {
			error "Target '$target' is parent/child form, but '$dir' is not a directory or library target"
		}

		library {
			if { [lindex $parts 1] ni {static shared} } {
				error "Target '$target' is a library, but specialization '[lindex $parts 1]' is unknown (expected: static or shared)"
			}
			return $parts
		}

		directory {
			# Simply go on.
		}
	}

	# Ok, having that confirmed, synthesize the target
	vlog "--- Synthesizing subdir-redirection target '$target'"
	set subtarget [file join {*}[lrange $parts 1 end]]

	set foreigntarget [pget agv::foreign($target)]

	set agv::target($target) [plist {
		name $target
		type custom
		command "%submake -C $dir $subtarget"
	}]

	# Take the output from the foreign target. Some processing parts
	# may rely on the foreign target's data.
	if { $foreigntarget != "" } {
		set output  [dict:at $foreigntarget output]
		if { $output != "" } {
			vlog " --- importing foreign target's output (in $dir): $output"
			set output [file join $dir $output]
			dict set agv::target($target) output $output
		}
	}
	return $target
}

proc agp-prepare-database {target {parent ""}} {
	vlog "--- Preparing database for target '$target'"

	# Make sure that the profile contains at least the "general"
	# key with empty contents.

	if { ![info exists agv::profile(default)] } {
		set agv::profile(default) ""
	}

	# Auto-generate targets "all" and ".", if not defined
	if { $target == "all" || $target == "." } {
		if { ![info exists agv::target(all)] } {
			vlog "--- Synthesizing 'all' and '.' targets"
			agv::p::PrepareGeneralTargets
		} else {
			# The user probably has defined the 'all' target on their own.
			# Check if the "reconfigure" target has been added to deps,
			# because the user was very likely to have forgotten it.

			# dict update is very useful. Syntax is:
			# dict update <dict-variable> (<keyname> <refvarname>)... <body>
			# This defines a series of <refvarname> which refers to a key in the dictionary.
			# Inside the body you can read and write the <refvarname> and this will
			# reflect the changes under given <keyname> in the dict.
			# The scope of the <refvarname> variable is only within <body>.

			dict update agv::target(all) depends d {
				if { "reconfigure-ifneeded" ni $d } {
					# dict lappend could be nice, but this is not what we want.
					# reconfigure must be first
					set d [concat reconfigure-ifneeded $d]
				}
			}
		}
	}

	if { [CheckDefinedTarget $target] == "" } {
		set par ""
		if { $parent != "" } {
			set par " (as a dependency of $parent)"
		}
		puts stderr "+++ AG ERROR: No such target: $target$par"
		return false
	}

	set targets_depends [dict:at $agv::target($target) depends]
	vlog "PROCESSING DEPENDS of $target (DIRECT): $targets_depends \{"

	foreach dep $targets_depends {
		lassign [CheckDefinedTarget $dep] dep spec
		vlog " ... DEP OF '$target': '$dep'"
		if { ![agp-prepare-database $dep $target] } {
			return false
		}
	}

	vlog "\} END DEPENDS OF $target"

	set type [dict:at $agv::target($target) type]
	set cat [dict:at $agv::target($target) install]

	if { $type == "" } {
		set type [DetectType $target]
		if { $type == "" } {
			error "Can't recognize target type for '$target' - please declare explicitly"
		}

		# If the target type had to be autodetected, change it to
		# phony if command key is also defined. This is only in case
		# when the type is not set - if the type was explicitly set
		# as phony, leave it as is.
		if { $type == "phony" && [dict exists $agv::target($target) command] } {
			set type custom
		}
	} else {
		lassign [split $type .] ncat ntype
		if { $ntype != "" } {
			# ignore category if type specified this way
			set cat $ncat
			set type $ntype
		}
	}

	# If category still not set, set default
	if { $cat == "" } {
		set cat noinst
	}

	# Write back to the database
	dict set agv::target($target) type $type
	dict set agv::target($target) install $cat

	vlog "Preparing database for '$target' type=$type install=$cat"

	set tarlang [dict:at $agv::target($target) language]

	# May happen that the target has already set language.
	# If so, select only those frameworks that are defined for that language.
	# Otherwise just use the general frameworks
	if { $tarlang == "" } {
		set tarlang default
	}

	# Note that the statements from 'default' are then spread to all
	# languages, unless particular setting for particular language is already set.
	# It means that if there was something set for general, it will be returned
	# also when asking for particular language.
	set globfw [pget agv::profile($tarlang).frameworks]
	set frameworks [dict:at $agv::target($target) frameworks]

	# Now the trick is how to join elements, make them unique, but preserve the order.
	set frameworks [StableIntersection $frameworks $globfw]

	# XXX default frameworks
	if { $frameworks == "" } {
		set frameworks pkg-config
	}

	# Set precalculated frameworks to the overall target's frameworks
	dict set agv::target($target) frameworks $frameworks

	if { [catch {ExecuteFrameworks $target prepare} result] } {
		puts stderr "Error reported from a framework - can't prepare database"
		return false
	}

	# XXX HERE PROCESSING SHOULD REGARD THE DEPENDENT TARGETS
	# (or otherwise some important data required by the parent target will not be generated)
	vlog " ... Processing '$target' as '$type'"
	# The 'Process:*' functions are expected to use the existing
	# data in the target database to define build rules.

	# This should be broken down into several steps: XXX
	# 1. For all targets, review the depends and check if there are any
	#    targets dependent on some targets in another directory. If so, load
	#    every directory. NOTE: Having "ag-subdir" command may be not necessary.
	#    Just make any target dependent on a target that is in a subdirectory,
	#    this will be noted automatically. Every such directory should be "loaded"
	#    with appropriate context. NOTE: Targets that are in-directories undergo
	#    basic data completion processing (to extract -output key), but NOT any
	#    other processing! 
	# 2. Complete basic data (add keys that should be there and are lacking).
	#    Generation of C/C++ file dependencies should also happen exactly here.
	#    

	# Catch the error to display a better error message
	if { [info command Process:$type] == "" } {
		error "Unknown target type: '$type' for target '$target'. The type must be either builtin or framework-provided."
	}

	Process:$type $target

	$::g_debug "DATABASE for '$target' AFTER PROCESSING:"
	DebugDisplayDatabase agv::target $target

	return true
}

# The path should begin with
# either // (then without any colons inside), or //b: or //s: or //t:
# or even with a custom prefix //NAME: if agv::NAMEdir is defined. Note
# that Windows path such as C:/foreign/source/late.cc does not treat C:
# as prefix because it doesn't start from // anyway. UNC paths are therefore
# not supported, you should mount the path to a local drive.
proc ag-instantiate {source {target ""} {varspec @}} {

	# The source file is stated to be relative to source directory,
	# but 'target' is stated to be relative to build directory.
	# If the target is specified and the path doesn't start from //,
	# the //b: is added

	if { $target == "" } {
		if { [file extension $source] == ".in" } {
			set target [file rootname $source]
		} else {
			error "ag-instantiate: Can't guess target for '$source', please specify explicitly"
		}
	}

	vlog "Instantiate: $source -> $target"

	set realsource [RealSourcePath $source]

	set wd [pwd]
	cd $agv::builddir

	# Use ResolveOutput1 to provide the correct path to the target file.
	# Exceptionally, this file is by default to be put into the build directory,
	# just as all output files do.
	# Users may override this default location by using the explicit // prefix.
	set realtarget [file normalize [ResolveOutput1 $target b]]
	cd $wd

	set fd [open $realsource r]
	set contents [read $fd]
	close $fd

	set EXPR {@([A-Za-z0-9_]+)@}

	set varspec [split $varspec ""] ;# convert string to list of characters

	if { "@" in $varspec } {
		# subst doesn't have -nocomplain or kind-of -onerror
		# For this simple case we can just list the used variables and defined them
		# The [dict values] command also makes the variable names unique
		set variables [dict values [regexp -all -inline $EXPR $contents]]
		foreach v $variables {
			if { ![uplevel [list info exists $v]] } {
				upvar $v r_$v
				# XXX Maybe some option that defines what to do if not found:
				# - replace with empty string (as here)
				# - keep the value (set the variable dereference expression to the value)
				#set r_$v "@$v@"
				set r_$v ""
			}
		}

		if { "$" ni $varspec } {
			# Protect normal $x expressions against substituting
			set contents [string map {$ ~$~} $contents]
		}

		# Replacing @VAR@ with ${VAR} is not that simple :)
		set contents [regsub -all $EXPR $contents {${\1}}]
	}

	#puts stderr "------------ ORIGINAL TEMPLATE --------------"
	#puts stderr $contents
	#puts stderr "---------------------------------------------"

	# Now instantiate variables - use the environment
	# from the place where the function was called
	set contents [uplevel [list subst -nocommands -nobackslashes $contents]]

	if { "$" ni $varspec } {

		# Replace back the previous dollar expressions
		set contents [string map {~$~ $} $contents]

	}

	set reltarget [prelocate $realtarget $agv::builddir $agv::toplevel]

	puts stderr "+++ Instantiating '[prelocate $realsource $agv::builddir]' into '$reltarget'"

	set tardir [file dirname $realtarget]
	if { ![file exists $tardir] } {
		file mkdir $tardir
	} elseif { ![file isdirectory $tardir] } {
		error "ag-instantiate: PATH '$realtarget' cannot be created (file in the way)"
	}

	set fd [open $realtarget w]
	# Would put some comment with information who has
	# instantiated that, however it can be any kind of file.
	puts $fd $contents
	close $fd

	# DON'T generate target under the $target name. Targets that have
	# slash inside are treated as target inside a directory and will cause
	# generation of redirection instead of generation in place.
	# XXX Shouldn't this use the -imgen option from profile?
	set target_name [GenFileBase path ag-instantiate $reltarget]

	# Prevent target duplication
	if { [info exists agv::target($target_name)] } {
		set numdia 0
		while { [info exists agv::target(${target_name}$numdia)] } {
			incr numdia
		}
		set target_name ${target_name}$numdia
	}

	# Now that it has been instantiated, mark this file as target to be cleaned.
	# Simply add generation target for it, without target
	#ag $target_name -type custom -output $target -s $source $::agfile -flags noclean distclean -command [list %error File '$target' can be only regenerated by 'reconfigure'.]
	ag-declare-generated $realtarget
}

proc ag-declare-generated {filename {wdstate b}} {

	# The output will have to be specified with the build directory,
	# however it will be also used as a source, so it must be explicitly stated.
	set f [ResolveOutput $filename $wdstate]
	$::g_debug "DECLARE GENERATED: $filename (registered as $f)"
	lappend agv::generated_files $f
}

proc ag-subdir args {
	if { [llength $args] == 1 } {
		set args [lindex $args 0]
	}

	foreach a $args {
		vlog "*** ANALYZING SUBDIR: $a"
		ag-subdir1 $a
	}
}

proc ag-subdir1 target {
	set target_dir [RealSourcePath $target]
	set ttype [file type $target_dir]

	switch -- $ttype {
		directory {
			set agfile ""
		}

		file {
			set agfile [file tail $target_dir]
			set target [file dirname $target_dir]
		}
	}

	ag $target -type directory -flags distclean
	if { $agfile != "" } {
		ag $target -agfile $agfile
	}

	lappend agv::p::directories $target

	set osd $agv::srcdir
	set od $agv::statedir
	set sd [prelocate [file join $od $target] . $agv::toplevel]  ;# may change ./tar into tar
	set local_builddir [file normalize [file join $agv::builddir $sd]]
	#puts stderr "SUBDIR: sd=$sd od=$od wd=[pwd] builddir=[pget agv::builddir]"
	if { [file exists $local_builddir] } {
		if { ![file isdirectory $local_builddir] } {
			error "File '$local_builddir' is blocking from creating a directory!"
		}
	} else {
		file mkdir $local_builddir
	}

	set target_dir_abs [file normalize $target_dir]

	# XXX This may need 'mkdir' in case of shadow build
	set wd [pwd]
	cd $local_builddir
	$::g_debug "<Entering> [pwd]"
	set agv::statedir $sd

	vlog "*** Trying $agfile in $target_dir"
	set agfile [FindSilverFile $agfile $target_dir_abs]

	if { $agfile == "" } {
		error "Can't process directory target '$sd': no Makefile.ag file found!"
	}

	set agfilepath_abs [file join $target_dir_abs $agfile]
	set agfilepath [prelocate $agfilepath_abs]
	$::g_debug "AG-SUBDIR: using directory '$sd'. Descending into Silverfile: '$agfilepath'"

	#mkv::p::run {*}[agv::AG] $agv::runmode -f $agfilepath -t $agv::toplevel
	set cmd_exe [agv::AG]
	lappend cmd_args $agv::runmode -f $agfilepath_abs -t $agv::toplevel

	# Add command-line variables to child commandlines
	foreach {name value} $::g_variables {
		lappend cmd_args "$name=$value"
	}

	interp create ag-interp-indir

	# XXX Pass the config and profile databases?
	ag-interp-indir eval set argv0 $::argv0
	ag-interp-indir eval set argv [list $cmd_args]
	ag-interp-indir eval set me_is_slave 1

	# Export declared private variables
	ag-interp-indir eval [list namespace eval agv {namespace eval p {}}]
	foreach v $agv::p::subdir_export {
		#puts stderr "DEBUG export:\t$v = [set $v]"
		ag-interp-indir eval [list set $v [set $v]]
	}
	#puts stderr "DEBUG: SUBDIR processing: $sd"
	#puts stderr "DEBUG: PRIVATE VARIABLES:"
	#foreach vv [ag-interp-indir eval info vars agv::p::*] {
	#	puts stderr "DEBUG:\t$vv = [ag-interp-indir eval set $vv]"
	#}

	# XXX THIS SKIPS OPTIONS - must be somehow supported!
	ag-interp-indir eval source [lindex $cmd_exe 0]

	$::g_debug "AG-SUBDIR: Silverfile from subdirectory '$sd' processed. Pinning in database."
	set imported_targets ""
	foreach t [ag-interp-indir eval array names agv::target] {
		set agv::foreign($target/$t) [ag-interp-indir eval set agv::target($t)]
		lappend imported_targets $target/$t
	}

	$::g_debug "AG-SUBDIR: Imported database:"
	foreach t $imported_targets {
		$::g_debug " --- $t (type: [pget agv::foreign($t).type])"
	}

	interp delete ag-interp-indir

	# Restore environment

	#set agv::srcdir $osd
	cd $wd
	$::g_debug "<Back in> [pwd]"
	set agv::statedir $od
}

proc ag-export names {
	foreach n $names {
		# Check if it's a procedure
		if { [info proc $n] == $n } {
			set procspec [list $n [info args $n] [info body $n]]
			#$::g_debug "EXPORTING PROC: $procspec"
			if { [lsearch -exact -index 0 $agv::p::exported_proc $n] == -1 } {
				lappend agv::p::exported_proc $procspec
			}
			continue
		}

		# Export a variable. Read the variable from the above stack frame
		upvar $n var
		if { ![info exists var] } {
			error "ag-export: $n is neither a procedure nor a variable"
		}
		if { [lsearch -exact -index 0 $agv::p::exported_var $n] == -1 } {
			lappend agv::p::exported_var [list $n $var]
		}
	}
}

package provide ag $agv::version
set g_debug mkv::p::pass
set ag_debug_on 0

# Default ag lib path
set def_ag_lib_path ""
if { !$tcl_interactive } {
	set here [file dirname $argv0]
	set path1 [file join $here .. lib silvercat]
	set path2 [file join $here .. share silvercat]

	if { [file isdirectory $path1] } {
		lappend def_ag_lib_path [file normalize $path1]
	}

	if { [file isdirectory $path2] } {
		lappend def_ag_lib_path [file normalize $path2]
	}
}

if { [info exists ::env(AG_LIB_PATH)] } {
	foreach p $::env(AG_LIB_PATH) {
		lappend agv::libpath [file nornalize $p]
	}
} elseif { $def_ag_lib_path != "" } {
	set agv::libpath $def_ag_lib_path
}

set wd [pwd]

foreach p $agv::libpath {
	cd $p
	if { [file exists startup] } {
		source startup
	}
}

cd $wd

if { !$tcl_interactive } {

set agfile {}
set help 0
set ag_debug_on 0
set g_debug mkv::p::pass
set agfiledir ""
set topdir ""
set runmode genrules
set install_prefix ""
set profile_overrides ""
set config_overrides ""
set readdbspec ""

#puts stderr "CURRENT DIR: [pwd]"
set agv::builddir [pwd]


set ag_optargs {
	-f agfile
	-help *help
	--help *help
	-v *verbose
	-d *ag_debug_on
	-C agfiledir
	-t topdir
	-m runmode
	-p -profile_overrides
	-c -config_overrides
	-r %readdbspec
	--prefix install_prefix
}

set opt_help {
	-f "<agfile>" "Set input filename (or path)"
	-v "" "Enforce verbose messages"
	-d "" "Enforce DEBUG messages"
	-C "<path>" "Use given path as base directory"
	-t "<dir>" "Use base toplevel directory (used by sub-cats)"
	-m "<runmode>" "Use given runmode: genrules(default) or make"
	-p "<key> <val>..." "Override settings provided from a profile"
	-c "<key> <val>..." "Override settings provided from a config file"
	-r "<agfile> ?key?..." "Read and print database entries from a file"
	--prefix "<path>" "Enforce install prefix (same as -p install:prefix <path>)"
}

lassign [process-options $argv $ag_optargs] g_args g_variables g_longoptions

if { $help } {
	puts "Usage: [file tail $::argv0] ?options? <target>"
	puts stderr "Options:"
	foreach {opt arg desc} $opt_help {
		puts stderr [format "  %-20s %s" "$opt $arg" $desc]
	}
	exit 1
}

if { $readdbspec != "" } {
	if { [llength $readdbspec] < 1 } {
		puts stderr "Usage: [file tail $::argv0] -r <filename> ?key?... (empty key for listing)"
		exit 1
	}

	set filename [lindex $readdbspec 0]
	set keys [lrange $readdbspec 1 end]

	if { [catch {set fd [open $filename r]}] } {
		puts stderr "ERROR: File not found: $filename"
		exit 1
	}

	set con [read $fd]
	close $fd

	if {$keys == ""} {
		puts [dict keys $con]
		exit 0
	}

	set output ""

	if { [dict exists $con {*}$keys] } {
		puts [dict get $con {*}$keys]
		exit 0
	}

	puts stderr "ERROR: key not found: $keys"
	exit 1
}

# Resolve ambiguous options -C and -f

if { $agfile != "" && $agfiledir != "" } {
	vlog "Passed both dir '$agfiledir' and file '$agfile'"
	set f [file join $agfiledir $agfile]
	set ok [expr {[file isfile $f] && [file isdirectory $agfiledir]}]
	if { !$ok } {
		puts stderr "Options -C and -f together provide directory and filename respectively"
		puts stderr "Using -C dir -f file is the same as using -f dir/file"
		puts stderr "Path does not lead to an existing file: $f"
		exit 1
	}
} elseif { $agfile != "" } {
	vlog "Passed only file '$agfile'"
	set agfiledir [file dirname $agfile]
	set agfile [file tail $agfile]
	vlog " -- spread to directory '$agfiledir' and file '$agfile'"
} else {
	vlog "Passed only directory: $agfiledir -- file itself will be guessed"
}

if { $agfiledir == "" } {
	set agfiledir .
}

if { $ag_debug_on } {
	set g_debug mkv::p::debug
	set mkv::debug mkv::p::debug
}

set mkv::p::verbose $verbose
set mkv::directory $agv::builddir

# --prefix LOCATION is alias to -p install:prefix LOCATION
if { $install_prefix != "" } {
	#puts stderr "DEBUG adding install:prefix: $install_prefix"
	dict set profile_overrides install:prefix $install_prefix
}

# Set this to global so that it's derived by subdirs
set agv::p::profile_overrides [dict merge $agv::p::profile_overrides $profile_overrides]
#puts stderr "DEBUG installing profile override: $agv::p::profile_overrides"


foreach {ln lv} $g_longoptions {
	set keyname [lassign [split $ln -] entry]
	set keyname [join $keyname -]

	switch -- $entry {
		avail {
			# Avail: declare that an option with given name occurs
			# in several alternatives, of which 'use' can select one.
			# Example: ag-config avail crypto {openssl gnutls}
			ag-config avail $keyname $lv
		}

		use {
			# Use one of alternatives (if declared with avail) or
			# use istallation path of a dependent library or package.
			# Example: ag-config use crypto gnutls
			ag-config use $keyname $lv
		}

		enable {
			if { $lv == "" } {
				set lv 1
			}
			if { ![string is boolean $lv] }  {
				error "For --$ln allowed value is a boolean value only"
			}
			ag-config enable $keyname $lv
		}

		disable {
			if { $lv == "" } {
				set lv 1
			}
			if { ![string is boolean $lv] }  {
				error "For --$ln allowed value is a boolean value only"
			}
			ag-config enable $keyname [expr {!$lv}]
		}

		have {
			if { $lv == "" } {
				set lv 1
			}
			if { ![string is boolean $lv] } {
				error "For --$ln allowed value is a boolean value only"
			}
			ag-config have $keyname $lv
		}

	    default {
			# Blindly add this as user option override.
			# This will be checked later.
			dict set agv::useropt_override $ln $lv
	    }
	}
}

vlog "Makefile will be generated in $mkv::directory"

# XXX something special about g_debug_on ?

foreach {name value} $g_variables {
    #puts "Setting $name to $value"
    set $name $value
}

set agfiledir_a [file normalize $agfiledir]

set agv::srcdir $agfiledir_a
#puts stderr "Setting srcdir as '$agv::srcdir'"

# Use the previous value, should that be set by option
set agfile [FindSilverFile $agfile $agfiledir_a]
if { $agfile == "" } {
	puts stderr "Can't find any Silverball. Bailing out."
	exit 1
}

set ag_call [list $::argv0 -f %agfile]


#puts stderr "AGFILE FOUND AS: $agfile"

set agv::runmode $runmode

set wd [pwd]
cd $agfiledir
$::g_debug "<Entering> [pwd]"

# Set this to current directory because this is the directory
# where all things happen.
set agv::builddir $wd
if { $topdir != "" } {
	# Check if toplevel directory is a parent to [pwd]
	set topdir [file normalize $topdir]
	set p1 [file split [pwd]]
	set p2 [file split $topdir]
	foreach x $p1 y $p2 {
		if { $x != $y } {
			# Check it it's because p2 has expired
			if { $y != "" } {
				puts stderr "ERROR: -t: '$topdir' is not a parent for '[pwd]'"
				exit 1
			}
		}
	}
	set agv::toplevel $topdir
	lappend ag_call -t %topdir
} else {
	set agv::toplevel [pwd]
}

puts stderr "+++ Reading database: [prelocate [pwd] $agv::toplevel]"
#puts stderr "DEBUG: existing profile overrides: $profile_overrides"

# Do sourcing in the original directory of the file.
# This is important so that all file references are relative
# to the directory in which this file resides.

# Tracing every written file would be tempting, but it's way too radical.
#trace add execution open leave agv-notify-filewrite-handler
source $agfile
#trace remove execution open leave agv-notify-filewrite-handler

foreach {k v} $agv::p::profile_overrides {
	# Not using ag-profile because we need to overwrite the value
	dict set agv::profile(default) $k $v
}

#puts stderr "DEBUG: RESULTING PROFILE (general):"
#parray agv::profile
#foreach {k v} $agv::profile(default) {
#	puts [format "%20s = %s" $k $v]
#}

puts stderr "+++ Processing runmode '$agv::runmode':  @[prelocate [pwd] $agv::toplevel]"

#set mkv::directory .

set exitcode [ag-do-$agv::runmode [lrange $g_args 1 end]]

if { ![info exists me_is_slave] } {
	exit $exitcode
}


} ;# END OF INTERACTIVE
