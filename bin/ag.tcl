#!/usr/bin/tclsh

# Get definitions of make.tcl as library
# XXX (find some better way to do it)
set was_interactive $tcl_interactive
set tcl_interactive 1
set gg_makepath [file dirname [info script]]/make.tcl
source $gg_makepath
set tcl_interactive $was_interactive

# Redefine MAKE so that the correct path is used
namespace eval mkv {
	proc MAKE {} {
		return [file normalize $::gg_makepath]
	}
}

namespace eval agv {
	set version 0.1 ;# just to define something

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

		set exported_proc ""
		set exported_var ""
	}

	set exe $p::exefor([info sharedlibextension])
	namespace export exe

	# Profile is an array indexed by language name.
	variable profile
	namespace export profile

	# Install prefix.
	# Will be set to the profile's default, unless overridden.
	proc prefix {} {
		return [ag-profile general ?prefix]
	}

	# Per-file set information
	variable fileinfo
	namespace export fileinfo

	# Private variable of genrules. Marks targets for which
	# the rules have been already generated.
	variable genrules_done ""
	namespace export genrules_done

	variable srcdir
	variable statedir .
	namespace export { srcdir statedir }
}

namespace import agv::p::dict:at
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

proc GetUnifiedProfile-cflags lang {
	set defines_flag [pget agv::profile($lang).defineflag]
	set incdir_flag [pget agv::profile($lang).incdirflag]
	#set libdir_flag [pget agv::profile($lang).libdirflag]

	set cflags [pget agv::profile($lang).cflags]

	foreach flagtype {defines incdir} {
		foreach val [pget agv::profile($lang).$flagtype] {
			set e [set ${flagtype}_flag]$val
			if { $e ni $cflags } {
				lappend cflags $e
			}
		}
	}

	$::g_debug " --- Profile-defined cflags for $lang: $cflags"

	return $cflags
}

proc GetUnifiedProfile-ldflags lang {

	set libdir_flag [pget agv::profile($lang).libdirflag]

	set ldflags [pget agv::profile($lang).ldflags]

	set ldflags_front ""
	foreach val [pget agv::profile($lang).libdir] {
		set e [set libdir_flag]$val
		if { $e ni $ldflags } {
			lappend ldflags_front $e
		}
	}
	set ldflags [concat $ldflags_front $ldflags]

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

	set defines_flag [pget agv::profile($lang).defineflag]
	set incdir_flag [pget agv::profile($lang).incdirflag]
	set libdir_flag [pget agv::profile($lang).libdirflag]

	set defines [dict:at $db defines]
	set incdir [dict:at $db incdir]
	set libdir [dict:at $db libdir]

	set cflags [dict:at $db cflags]

	foreach flagtype {defines incdir} {
		foreach val [dict:at $db $flagtype] {
			set e [set ${flagtype}_flag]$val
			if { $e ni $cflags } {
				lappend cflags $e
			}
		}
	}

	set ldflags [dict:at $db ldflags]
	set need_ldflags ""

	foreach val [dict:at $db libdir] {
		set e ${libdir_flag}$val
		if { $e ni $ldflags } {
			lappend need_ldflags $e
		}
	}

	if { $need_ldflags != "" } {
		set ldflags [concat $need_ldflags $ldflags]
	}

	#puts stderr "($target) Extra cflags from special options: $cflags (from defines: $defines incdir: $incdir lang: [dict:at $db language])"

	dict set agv::target($target) cflags $cflags
	dict set agv::target($target) ldflags $ldflags
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
proc CreateLibraryFilename {target type} {
	if { $type == "runtime" } {
		return lib$target.so
	}

	return lib$target.a
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
		# Get all language names except "default"
		set name [array names agv::profile]
		set name [lsearch -all -inline -not $name default]
		set name [lsearch -all -inline -not $name structure]
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

proc InstallProfile {name} {

	if { $name in {default structure} } {
		error "Invalid profile name (keyword)"
	}

	if { ![dict exists $agv::p::profiles $name] } {
		error "No such profile: $name"
	}

	set prof ""
	if { ![array exists agv::profile] } {
		set prof [dict get $agv::p::profiles default]
		$::g_debug "FRESH PROFILE: adding defaults: $prof"
	} else {
		$::g_debug "EXISTING PROFILE: [array get agv::profile]"
	}
	set newprof [dict get $agv::p::profiles $name]
	$::g_debug "ADDING PROFILE CONTENTS: $newprof"

	# Merge manually - official merging would overwrite keys and therefore
	# possibly delete sub-keys, while we want
	# to merge sub-keys.
	array set tmp_prof $prof
	foreach {key val} $newprof {
		set tmp_prof($key) [dict merge [pget tmp_prof($key)] $val]
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

	# Now merge every language item with default.
	# Leave the default untouched, however.

	set deflt [dict:at $prof default]
	foreach lng [dict keys $prof] {
		if { $lng == "default" } {
			continue
		}

		dict set prof $lng [dict merge [dict:at $prof default] [dict:at $prof $lng]]
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
		o { return output }
		I { return incdir }
		D { return defines }
		L { return libdir }
		libs { return ldflags }
		lflags { return ldflags }
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

	set singles [pget agv::p::singles]

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

		if { !$nomoreoptions && $f2 == "+ " } {
			set o [string range $o 2 end]
		}

		if { !$nomoreoptions && $f3 == "++ " } {
			set o [string range $o 3 end]
			set push_front true
		}

		$::g_debug "TO SET '$lastopt' (before): $o"
		set o [puncomment $o]
		set o [pexpand $o 3]
		$::g_debug "TO SET '$lastopt' (after ): $o"
		# This time it's nothing special. At least try to strip one level,
		# in case when user did -option {value1 value2}
		if { ![catch {llength $o} size] } {
			if { $size == 1 } {
				set o [lindex $o 0]
			}
			if { $push_front } {
				set options($lastopt) [concat $o $options($lastopt)]
			} elseif { $lastopt in $singles } {
				if { [info exists options($lastopt)] } {
					vlog " +++ $lastopt is expected as single - OVERRIDING existing value '$options($lastopt)' with $o"
				}
				set options($lastopt) $o
			} else {
				lappend options($lastopt) {*}$o
			} 
		} else {
			# Happened to be a text not convertible to a list.
			# Well, happens. Just append to the existing value.
			if { [info exists options($lastopt)] } {
				if { $push_front } {
					set options($lastopt) "$o $options($lastopt)"
				} else {
					append options($lastopt) " $o"
				}
			} else {
				set options($lastopt) $o
			}
		}
		# Do nothing in case when calculating lenght resulted in exception.

		$::g_debug " --- --- AC/DB: $lastopt += {$o}"
	}

	if { $query != "" } {
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
	set tar [file join $agv::statedir $target]
	set target [prelocate $tar]

	return [AccessDatabase agv::target $target {*}$args]
}

proc ag-info {filename args} {
	return [AccessDatabase agv::fileinfo $filename {*}$args]
}

proc ProcessLanguage target {
	set db $agv::target($target)

	set sources [dict:at $db sources]

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
		set lang [agv::p::GetCommonLanguage [dict keys $used_langs]]
		dict set db language $lang
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

	set used_langs ""

	vlog "Performing general processing for [dict:at $db type] '$target':"
	$::g_debug "SOURCES: $sources"

	set hdrs ""
	foreach s $sources {

		set info [pget agv::fileinfo($s)]
		set lang [dict:at $info language]

		set depspec [dict:at $agv::profile($lang) depspec]
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

	foreach s $sources {

		set info [pget agv::fileinfo($s)]

		vlog "INFO($s): $info"

		# Now that $s is used ONLY AS FILENAME (NOT IDENTIFIER!)
		# it can be now localized towards build directory
		
		set s_abs [RealSourcePath $s]
		set s [prelocate $s_abs $agv::builddir]

		set lang [dict:at $info language]
		set depspec [dict:at $agv::profile($lang) depspec]
		set depopt [dict:at $agv::profile($lang) depopt]
	    set imgen [dict:at $db imgen]
	    if { $imgen == "" } {
			set imgen [dict:at $agv::profile($lang) imgen]
		}
	    if { $imgen == "" } {
	    	set imgen path
	    }

		# XXX This is required just to make things work, but it may be
		# required that this be optional, and "original directory preserved"
		# for o-files should be somehow supported.
		set o [GenFileBase $imgen $target $s].ag.o

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
					set rule "$s {\n\t!tcl gendep [prelocate $agv::srcdir $agv::builddir] $depfile $depcmd\n}"
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
				lappend deps [prelocate [RealSourcePath $i] $agv::builddir]
			}
		}

		$::g_debug "DEPS: $deps"

		# Add rule to objects, notify in the database
		if { [lsearch $objects $o] == -1 } {
			lappend objects $o
		}

		if { ![info exists mkv::generated($o)] } {

			# Generate rule for the target
			set rule [GenerateCompileRule $db $lang $o $s $deps]

			# Don't override rules that are set already.
			if { ![dict exists $rules $o] } {
				dict set rules $o $rule
			}

			set mkv::generated($o) [list $target $s $rule]
		} else {
			# Generate rule for the target
			set rule [GenerateCompileRule $db $lang $o $s $deps]
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

	ProcessCompileLink program $target $outfile
	Process:phony $target
}

proc Process:library target {

	# NOTE: This immediate "archive" should be extracted
	# from "libtype" key.
	set outfile [pget agv::target($target).output]
	if { $outfile == "" } {
		set outfile [CreateLibraryFilename $target archive]
		dict set agv::target($target) output $outfile
	}

	ProcessCompileLink library $target $outfile
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

	set db $agv::target($target)

	set rules [dict:at $db rules]
	set phony [dict:at $db phony]
	set outfile [ResolveOutput [dict:at $db output]]
	set sources [dict:at $db sources]
	set command [dict:at $db command]

	

	set rsrc ""
	foreach s $sources {
		lappend rsrc [prelocate [RealSourcePath $s] $agv::builddir]
	}

	set rule [list {*}$rsrc "\n\t$command\n"]
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
	set agv::target($target) $db

	DebugDisplayDatabase agv::target $target

	Process:phony $target
}

proc GenerateInstallCommand {cat outfile prefix {subdir ""}} {
	set icmd ""

	switch -- $cat {
		noinst {
			# Nothing.
			set icmd "# Nothing to install for '$outfile' (noinst)"
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
				puts stderr "+++AG WARNING: No installdir for -install $cat - can't install $outfile"
			}
		}
	}

	return $icmd
}

proc GenerateInstallTarget:program {target prefix} {

	set db $agv::target($target)
	set outfile [ResolveOutput [dict:at $db output]]
	set icmd [GenerateInstallCommand [dict:at $db install] $outfile $prefix]
	if { $icmd == "" } {
		return
	}

	set itarget install-$target
	dict set db rules $itarget [list $outfile \n$icmd\n]
	dict set db phony $itarget ""
	# Ok, ready. Write back to the database
	set agv::target($target) $db
}

proc GenerateInstallTarget:library {target prefix} {
	set db $agv::target($target)

	set libfile [ResolveOutput [dict:at $db output]]
	set libicmd [GenerateInstallCommand [dict:at $db install] $libfile $prefix]

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

	dict set db phony install-$target-devel [list install-$target-archive install-$target-headers]

	# XXX Check for dynamic library !!!
	dict set db phony install-$target-runtime ""  ;# XXX Should refer to installing dynamic library
	dict set db phony install-$target [list install-$target-devel install-$target-runtime]

	# Ok, ready. Write back to the database
	set agv::target($target) $db
}

proc ProcessCompileLink {type target outfile} {

	# The ProcessLanguage does only language check for all sources
	# and defines the languages for every source file as well as for
	# the whole target. This is required prior to ProcessFlags, as
	# it will have to ask profile about prospective additional options.
	ProcessLanguage $target
	ProcessFlags $target
	ProcessSources $target

	# XXX BUG DEBUG - still a bug, remove after fixing
	set prev_outfile [pget agv::target($target).filename]
	$::g_debug " --**--++-- ProcessCompileLink: using output filename '$outfile' (was $prev_outfile) for $type $target"
	dict set agv::target($target) output $outfile

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
	set rule [GenerateLinkRule:$type $db $outfile]
	dict set rules $outfile $rule

	# Add a phony rule that redirects the symbolic name to physical file,
	# in case they differ (in future, this can be due to them being in different directories)

	set phony ""
	if { $outfile != $target } {
		vlog "Adding phony $target -> $outfile (because they differ)"
		dict set phony $target $outfile
	}

	# Ok, ready. Write back to the database
	dict set db rules $rules
	dict set db phony $phony
	set agv::target($target) $db

	#$::g_debug "DATABASE: agv::profile\[default\]"
	#DebugDisplayDatabase agv::profile default

	set prefix [dict:at $agv::profile(default) prefix]
	if { $prefix == "" } {
		puts stderr "+++ AG WARNING: 'prefix' not found - not generating install targets"
	} else {
		GenerateInstallTarget:$type $target $prefix
	}

	ExecuteFrameworks $target complete
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

proc GenerateCompileRule {db lang objfile source deps} {

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

	set depflags ""
	set depopt [dict:at $agv::profile($lang) depopt]
	if { [string index $deps 0] == "<" && $depopt != "" } {
		# Rule for deps wasn't generated then, although mentioned here.
		# It's predicted to generate depfile and object file in one step
		set depfile [string range $deps 1 end]
		set depflags "${depopt}$depfile"
		set deps "$source $deps"

		$::g_debug "ONESTEP dep for '$source': $deps ($source from $agv::srcdir relative to $agv::builddir)"
	}

	# Ok, now we need to readjust source and deps to be in
	# the srcdir
	$::g_debug "GENERATING FOR  $source: $deps"

	set command "$compiler $cflags $depflags $source $oflag $objfile"
	$::g_debug "... Command: $command"

	set rule "$deps {\n\t$command\n}"

	$::g_debug "... Generated rule: $rule"

	return $rule
}

proc GetTargetFile target {
	set filename [ResolveOutput1 [dict:at $agv::target($target) output]]
	if { $filename == "" } {
		$::g_debug "ERROR: no filename set in this database:"
		DebugDisplayDatabase agv::target $target
		error "Output file not defined for $target"
	}

	return $filename
	#set dir [file dirname $target]
	#return [file join $dir $filename]
}

proc ResolveOutput1 o {
	# Output files are expected to be:
	# - relative path: should be relative to builddir, keep it as is
	# - absolute path: keep it as is
	# - -- exception: path started with // is special!
	if { [string range $o 0 1] != "//" } {
		return $o
	}

	set rest [string range $o 2 end]
	lassign [split $rest :] prefix file
	if { $prefix == "" } {
		return ""
	} elseif { $file == "" } {
		set file $prefix
		set prefix "s"
	}

	set apath ""

	switch -- [string index $prefix 0] {
		s {
			set apath $agv::srcdir
		}

		b {
			set apath $agv::builddir
		}

		default {
			set var "agv::${prefix}dir"
			if { [info exists $var] } {
				set apath [set $var]
			}
		}
	}

	if { $apath == "" } {
		error "Invalid special-path specification: $prefix (in $o) - use 's:' or 'b:'"
	}

	set relpath [prelocate [file join $apath $file] $agv::builddir]

	return $relpath
}

proc ResolveOutput output {
	set out ""
	foreach o $output {
		lappend out [ResolveOutput1 $o]
	}
	return $out
}

proc GetDependentLibraryTargets target {
	set depends [dict:at $agv::target($target) depends]  
	$::g_debug "Getting dependent targets for '$target': $depends"
	set libs ""
	set langs ""

	foreach d $depends {
		if { ![CheckDefinedTarget $d] } {
			error "Target '$d' (dependency of $target) is not defined"
		}

		set type [dict:at $agv::target($d) type]

		# Check if this is an imported target
		if { [info exists agv::foreign($d)] } {
			set type [dict:at $agv::foreign($d) type]
			vlog "--- taking foreign target '$d' type '$type'"
		}

		if { $type == "library" } {
			set o [GetTargetFile $d]
			$::g_debug " -- LIBRARY: output=[pget agv::target($d).output] ($o)"
			lappend libs $o
			set langs [dict:at $agv::target($d) language]
			$::g_debug " --- Target '$d' provides libraries: $libs (language: $langs)"

			# Recursive call
			# XXX consider unwinding - recursion in Tcl is limited and may
			# result in internal error!
			lassign [GetDependentLibraryTargets $d] adlibs adlangs
			set libs [concat $adlibs $libs]
			lappend langs {*}$adlangs
		} else {
			$::g_debug " --- Target of type '$type' does not provide dependent libraries."
		}
	}

	return [list $libs $langs]
}

proc GenerateLinkRule:program {db outfile} {

	# Check dependent targets. If this target has any dependent targets of type library,
	# add its library specification to the flags.
	lassign [GetDependentLibraryTargets [dict:at $db name]] libs langs
	lappend langs {*}[dict:at $db language]
	set langs [lsort -unique $langs]
	set lang [agv::p::GetCommonLanguage $langs]
	vlog "GLR/p: Common language for '$langs': $lang"

	set objects [dict:at $db objects]
	set depends [dict:at $db depends]
	set ldflags [CompleteFlags $db $lang ldflags]


	set linker [dict get $agv::profile($lang) link]
	set oflag [dict get $agv::profile($lang) link_oflag]

	$::g_debug "Generating link rule for '$outfile' ldflags: $ldflags libs: $libs"

	set command "$linker $objects $oflag $outfile $libs $ldflags"

	# The rule should contain all ingredient files
	# and all "targets" declared here as its dependency
	# (be it phony or file-based target). The exact file that
	# needs to be used in the command is already extracted
	# by GetDependentLibraryTargets.
	set rule "$objects $depends {\n\t$command\n}"

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
	[mkv::MAKE] clean && !agcmd genrules -f !agfile !varexpr&& [mkv::MAKE] [pget mkv::targets]
	%exit
}

	# For genrules, add also reconfigure rule to regenerate Makefile.tcl.
	ag reconfigure -type custom -output Makefile.tcl -flags noclean -sources $::agfile \
		-command {[string map [list !agcmd [agv::AG] !agfile $agfile_inmake !varexpr $varexpr] $cmdf]}
		
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

	set rules [dict:at $agv::target($target) rules]
	set type [dict:at $agv::target($target) type]
	set flags [dict:at $agv::target($target) flags]

	# Rules is itself also a dictionary.
	# Key is target file, value is dependencies and command at the end.

	foreach {tarfile data} $rules {
		puts $fd "rule $tarfile $data"
		if { $flags != "" } {
			puts $fd "setflags $tarfile $flags"
		}
	}

	# First rules, then phony. Later phonies may override earlier rules.
	set phony [dict:at $agv::target($target) phony]

	if { $phony != "" } {
		foreach {rule deps} $phony {
			puts $fd "phony $rule $deps"
		}
	}

	puts $fd ""

	lappend agv::genrules_done $target

	# Now generate everything for the dependent targets
	set deps [dict:at $agv::target($target) depends]

	vlog "Makefile generator: will generate sub-rules for deps: $deps"
	foreach d $deps {
		#if { [IsSubTarget $d] } {
		#	vlog " --- skipping target in a subdir: $d"
		#	continue
		#}
		GenerateMakefile $d $fd
	}
	vlog "Makefile generator: finished sub-rules for deps"

	puts $fd [SynthesizeClean $target]

	return true
}

proc SynthesizeClean {target} {
	set type [dict:at $agv::target($target) type]

	set parts [file split $target]
	# use [join <parts> /] not [file join] because the latter
	# doesn't work with empty arguments!
	set dir [join [lrange $parts 0 end-1] /]
	set tarname [lindex $parts end]

	vlog "Makefile: checking for clean for '$tarname' in $dir ($type)"

	if { $tarname == "all" } {
		set cleanname clean
	} else {
		set cleanname $tarname-clean
	}

	if { $tarname != "all" && $type ni {program library custom} } {
		return "# Not synthesizing clean target for $type $target"
	} 

	# Ok, now the generation in a subdirectory should
	# forward it to the original makefile.
	if { $dir != "" } {
		vlog "Makefile generation: synthesizing '$dir/$cleanname' by forwarding to $dir"

		# Add it also to the list of clean targets for that target.
		# It's needed to spit it out as an overall clean target dependency.
		lappend agv::target($target) cleantarget $dir/$cleanname

		return "rule $dir/$cleanname {\n\t!tcl submake -C $dir $cleanname\n}\nphony $dir/$cleanname"
	}

	set cleandeps ""
	set cleanflags ""
	if { $tarname == "all" } {
		# This is a synthetic 'all' target, so take clean names from
		# all dependent targets, if any. They must be dependencies of clean.
		foreach d [ag all ?depends] {
			lappend cleandeps {*}[dict:at $agv::target($d) cleantarget]
		}
		set cleanflags "-noclean"
	}

	vlog "Makefile generating: synthesizing '$cleanname' target to clean '$target' (with extra $cleandeps)"
	set orule "rule $cleanname $cleandeps {\n\t%autoclean $target $cleanflags\n}\nphony $cleanname"
	if { $tarname == "all" } {
		append orule "\nrule distclean clean {\n\t%autoclean $target\n\t%autoclean $target distclean\n}"
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

proc ag-make {target} {
	set wd [pwd]
	file mkdir $agv::builddir
	cd $agv::builddir
	ag-do-make $target
	cd $wd
}

# This procedure should be used instead of checking if exists agv::target($target).
# It's specifically for targets that may have a special form and because of that
# have to be synthesized lazily.
# Currently it concerns only detecting "directory-based targets"
proc CheckDefinedTarget target {

	# If the target exists - it's already done.
	if { [info exists agv::target($target)] } {
		return true
	}

	# Maybe it's undefined because it's directory based.
	set parts [file split $target]
	if { [llength $parts] < 2 } {
		# Not a directory-based target, so it must be explicitly defined
		return false
	}

	# It is directory based.
	# So, check if directory target is defined
	set dir [lindex $parts 0]
	if { ![info exists agv::target($dir)] } {
		# XXX Maybe do lazy-define of the target?
		error "Target '$target' is subdir-based, but no target '$dir' is defined (use ag-subdir to define)"
	}

	if { [dict:at $agv::target($dir) type] != "directory" } {
		error "Target '$target' is subdir-based, but '$dir' is not a directory target"
	}

	# Ok, having that confirmed, synthesize the target
	vlog "--- Synthesizing subdir-redirection target '$target'"
	set subtarget [file join {*}[lrange $parts 1 end]]

	set foreigntarget [pget agv::foreign($target)]

	set agv::target($target) [plist {
		name $target
		type custom
		command "!tcl submake -C $dir $subtarget"
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
	return true
}

proc agp-prepare-database {target {parent ""}} {
	vlog "--- Preparing database for target '$target'"

	# Make sure that the profile contains at least the "general"
	# key with empty contents.

	if { ![info exists agv::profile(default)] } {
		set agv::profile(default) ""
	}

	# Auto-generate target "all", if not defined
	if { $target == "all" && ![info exists agv::target(all)] } {
		vlog "--- Synthesizing 'all' target"
		agv::p::PrepareGeneralTarget
	}

	if { ![CheckDefinedTarget $target] } {
		set par ""
		if { $parent != "" } {
			set par " (as a dependency of $parent)"
		}
		puts stderr "+++ AG ERROR: No such target: $target$par"
		return false
	}

	vlog "PROCESSING DEPENDS of $target"

	foreach dep [dict:at $agv::target($target) depends] {
		vlog " ... DEP OF '$target': '$dep'"
		if { ![agp-prepare-database $dep $target] } {
			return false
		}
	}

	vlog "END DEPENDS OF $target"

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
	Process:$type $target

	$::g_debug "DATABASE for '$target' AFTER PROCESSING:"
	DebugDisplayDatabase agv::target $target

	return true
}

proc ag-instantiate {source {target ""} {varspec @}} {

	if { $target == "" } {
		if { [file extension $source] == ".in" } {
			set target [file rootname $source]
		} else {
			error "ag-instantiate: Can't guess target for '$source', please specify explicitly"
		}
	}

	set fd [open $source r]
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

	$::g_debug "Instantiating $source into $target"

	set fd [open $target w]
	# Would put some comment with information who has
	# instantiated that, however it can be any kind of file.
	puts $fd $contents
	close $fd

	# DON'T generate target under the $target name. Targets that have
	# slash inside are treated as target inside a directory and will cause
	# generation of redirection instead of generation in place.
	set target_name [string map {../ _ ./ {} / -} $target]

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
	ag $target_name -type custom -output $target -s $source -flags noclean distclean -command [list %error "Regenerate '$target' only by 'ag.tcl genrules'"]
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

	ag $target -type directory
	if { $agfile != "" } {
		ag $target -agfile $agfile
	}

	lappend agv::p::directories $target

	set osd $agv::srcdir
	set od $agv::statedir
	set sd [prelocate [file join $od $target]]  ;# may change ./tar into tar
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
	lappend cmd {*}[agv::AG] $agv::runmode -f $agfilepath_abs -t $agv::toplevel
	interp create ag-interp-indir

	# XXX Pass the config and profile databases?
	ag-interp-indir eval set argv0 $::argv0
	ag-interp-indir eval set argv [list [lrange $cmd 1 end]]
	ag-interp-indir eval set me_is_slave 1
	ag-interp-indir eval source [lindex $cmd 0]

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

package provide ag 0.8
set g_debug mkv::p::pass
set ag_debug_on 0

if { !$tcl_interactive } {

set agfile {}
set help 0
set ag_debug_on 0
set g_debug mkv::p::pass
set agfiledir ""
set topdir ""

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
}

lassign [process-options $argv $ag_optargs] g_args g_variables

if { $help || [string trim $g_args] == "" } {
	puts "Usage: [file tail $::argv0] genrules <target>"
	puts stderr "Options:"
	foreach {opt arg} $ag_optargs {
		puts stderr [format "  %-8s %s" $opt: $arg]
	}
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

proc got_u_writer args {
	error "hands off!"
}

#trace variable mkv::directory w got_u_writer

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

set ag_call [list $::argv0 genrules -f %agfile]


#puts stderr "AGFILE FOUND AS: $agfile"

set agv::runmode [lindex $g_args 0] ;# != "" because g_args != ""

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

puts stderr "READING DATABASE @[prelocate [pwd] $agv::toplevel]"

# Do sourcing in the original directory of the file.
# This is important so that all file references are relative
# to the directory in which this file resides.
source $agfile

puts stderr "PROCESSING TO GENERATE MAKEFILE @[prelocate [pwd] $agv::toplevel]"

#set mkv::directory .

set exitcode [ag-do-$agv::runmode [lrange $g_args 1 end]]

if { ![info exists me_is_slave] } {
	exit $exitcode
}


} ;# END OF INTERACTIVE
