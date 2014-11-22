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

	# Install prefix.
	# Will be set to the profile's default, unless overridden.
	variable prefix
	namespace export prefix

	# Per-file set information
	variable fileinfo
	namespace export fileinfo

	# Private variable of genrules. Marks targets for which
	# the rules have been already generated.
	variable genrules_done ""
	namespace export genrules_done
}

namespace import agv::p::dict:at

proc GenerateCompileFlags lang {
	set define_flags ""

	set define_flags ""
	set defines [dict:at [pget agv::profile($lang)] defines]
	foreach def $defines {
		lappend define_flags -D$def
	}

	set inc_flags ""
	set incdir [dict:at [pget agv::profile($lang)] incdir]
	foreach def $incdir {
		lappend inc_flags -I$def
	}

	set cflags [dict:at [pget agv::profile($lang)] cflags]

	$::g_debug " --- Profile-defined cflags for $lang: DEF: $define_flags INC: $inc_flags OTHER: $cflags"

	return "$define_flags $cflags $inc_flags"
}

# This should do more-less the same as GenerateCompileFlags, just
# has to extract additional info from target's extra data (defines and incdir)
# and write them into cflags cell.
proc ProcessFlags target {
	set db $agv::target($target)

	set cflags [dict:at $db cflags]
	set defines [dict:at $db defines]
	set incdir [dict:at $db incdir]

	foreach d $defines {
		set e -D$d
		if { [lsearch -exact $cflags $e] != -1 } {
			lappend cflags $e
		}
	}

	foreach d $incdir {
		set e -I$d
		if { [lsearch -exact $cflags $e] != -1 } {
			lappend cflags $e
		}
	}

	dict set agv::target($target) cflags $cflags
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
#    Example: StableUniq {a b a} {b c d} -> {a b c d}
#  - if two elements occur in both lists, but in different order, the order of
#    the first list is preserved.
#    Example: StableUniq {a b d c} {a c e d} -> {a b d c e}
proc StableUniq {l2 l1} {

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

proc GenerateDepends {lang cflags source} {
	$::g_debug "Extracting ingredients of '$source' (cflags: $cflags)"

	set gendep [dict:at [pget agv::profile($lang)] gendep]
	if { $gendep == "" } {
		puts stderr "+++ Ag/ERROR: 'gendep' undefined for language $lang"
		error "Please complete profile for language $lang or define includes explicitly"
	}

	set wrapped_flags [ShellWrapAll $cflags]

	set cmd "$gendep [ShellWrapAll $cflags] $source"
	$::g_debug "Command: $cmd"

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
	}
	set prof [dict merge $prof [dict get $agv::p::profiles $name]]

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
		I { return incdir }
		D { return defines }
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

		# This time it's -option {- config speed} - means remove these items
		if { !$nomoreoptions && $f2 == "- " } {
			set o [pexpand [lrange $o 2 end] 3]
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

		set o [pexpand $o 3]
		# This time it's nothing special. At least try to strip one level,
		# in case when user did -option {value1 value2}
		if { ![catch {llength $o} size] } {
			if { $size == 1 } {
				set o [lindex $o 0]
			}
			if { $push_front } {
				set options($lastopt) [concat $o $options($lastopt)]
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

		$::g_debug " --- --- AC/DB: $lastopt += $o"
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
			$::g_debug " --- Autodetecting language for $s: $lang"
			dict set agv::fileinfo($s) language $lang
		}

		if { [string match *-header $lang] } {
			puts stderr "+++ Ag/ERROR: Target:$target Source:$s"
			puts stderr "+++ Ag/ERROR: File identified as header file ($lang)"
			error "A file identified as 'header' ($lang) cannot be a source file."
		}

		dict lappend used_langs $lang $s

		# Check if you have depends declared explicitly. If so, use them.
		set info [pget agv::fileinfo($s)]
		if { [dict exists $info includes] } {
			$::g_debug " --- Headers for '$s' are known - not generating"
			set deps [concat $s [dict get $info includes]]
		} else {
			$::g_debug " --- Include info not found for '$s' - using gendep to generate:"
			set cflags [CompleteCflags $db $lang]
			set deps [GenerateDepends $lang $cflags $s]
			# Write them back to the database
			dict set agv::fileinfo($s) includes [lrange $deps 1 end] ;# skip the source itself
			parray agv::fileinfo
		}
	}

	ExecuteFrameworks $target compile-pre $sources

	foreach s $sources {

		set info [pget agv::fileinfo($s)]
		
		vlog "INFO($s): $agv::fileinfo($s) = $info"

		set lang [dict:at $info language]

		set o [file rootname $s].ag.o
		$::g_debug " ... processing $s (language $lang) --> $o"

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


	set lang [dict:at $db language]
	if { $lang == "" } {
		set lang [agv::p::GetCommonLanguage [dict keys $used_langs]]
		dict set db language $lang
	}

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

	ProcessFlags $target
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

	$::g_debug " ... target's cflags: $cflags"
	$::g_debug " ... lang($lang) cflags: $lang_cflags"

	append cflags " $lang_cflags"

	return $cflags
}

proc GenerateCompileRule {db lang objfile source deps} {

	if { ![info exists agv::profile($lang)] } {
		error "Silvercat doesn't know how to compile '$lang' files.\nPlease select correct profile"
	}

	$::g_debug "Generating compile rule for '$objfile':"

	set cflags [CompleteCflags $db $lang]

	# The rule should be formed as for make.tcl
	# except the 'rule' command and the target name.
	# So: source-deps... command-to-compile

	set compiler [dict get $agv::profile($lang) compile]
	set oflag [dict get $agv::profile($lang) compile_oflag]

	set command "$compiler $cflags $source $oflag $objfile"
	$::g_debug "... Command: $command"

	set rule "$deps {\n\t$command\n}"
}

proc GetTargetFile target {
	set filename [dict:at $agv::target($target) filename]
	if { $filename == "" } {
		$::g_debug "ERROR: no filename set in this database:"
		DebugDisplayDatabase agv::target $target
		error "Filename not defined for $target"
	}

	set dir [file dirname $target]
	return [file join $dir $filename]
}

proc GetDependentLibraryTargets target {
	set depends [dict:at $agv::target($target) depends]  
	$::g_debug "Getting dependent targets for '$target': $depends"
	set libs ""
	set langs ""

	foreach d $depends {
		if { ![info exists agv::target($d)] } {
			error "Target '$d' (dependency of [dict:at $db name]) is not defined"
		}

		set type [dict:at $agv::target($d) type]

		if { $type == "library" } {
			lappend libs [GetTargetFile $d]
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
	set ldflags [dict:at $db ldflags]

	set linker [dict get $agv::profile($lang) link]
	set oflag [dict get $agv::profile($lang) link_oflag]

	$::g_debug "Generating link rule for '$outfile' ldflags: $ldflags libs: $libs"

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

	if { $target in $agv::genrules_done } {
		vlog "NOT Generating rules for $target -- already generated"
		return
	}

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

	lappend agv::genrules_done $target

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
	set globfw [dict:at $agv::profile($tarlang) frameworks]
	set frameworks [dict:at $agv::target($target) frameworks]

	# Now the trick is how to join elements, make them unique, but preserve the order.
	set frameworks [StableUniq $frameworks $globfw]

	# XXX default frameworks
	if { $frameworks == "" } {
		set frameworks pkg-config
	}
	
	# Set precalculated frameworks to the overall target's frameworks
	dict set agv::target($target) frameworks $frameworks

	if { ![ExecuteFrameworks $target prepare] } {
		return false
	}

	vlog " ... Processing '$target' as '$type'"
	Process:$type $target
	
	$::g_debug "DATABASE for '$target' AFTER PROCESSING:"
	DebugDisplayDatabase agv::target $target

	vlog "PROCESSING DEPENDS of $target"

	foreach dep [dict:at $agv::target($target) depends] {
		if { ![agp-prepare-database $dep] } {
			return false
		}
	}

	vlog "END DEPENDS OF $target"
	return true
}

proc ag-instantiate {source target {varspec @}} {
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
}

proc ag-subdir args {
	if { [llength $args] == 1 } {
		set args [lindex $args 0]
	}

	lappend agv::p::directories {*}$args
}

package provide ag 0.8

if { !$tcl_interactive } {

set agfile {}
set help 0
set ag_debug_on 0
set g_debug pass

set ag_optargs {
	-f agfile
	-help *help
	--help *help
	-v *g_verbose
	-d *ag_debug_on
}

lassign [mk-process-options $argv $ag_optargs] g_args g_variables

if { $help } {
	puts "Usage: [file tail $::argv0] genrules <target>"
	puts stderr "Options:"
	foreach {opt arg} $g_optargs {
		puts stderr [format "  %-8s %s" $opt: $arg]
	}
	exit 1
}

if { $ag_debug_on } {
	set g_debug debug
}


# XXX something special about g_debug_on ?

foreach {name value} $g_variables {
    #puts "Setting $name to $value"
    set $name $value
}

set possible_agfiles {Makefile.ag.tcl Makefile.ag makefile.ag.tcl makefile.ag} 

if { $agfile == "" } {
	foreach agfile [list {*}$possible_agfiles .] {
		# "dot" always exists - it's the current directory
		# buf even if it accidentally doesn't exist, it doesn't matter!
		if { [file exists $agfile] } break
	}

	if { $agfile == "." } {
		puts stderr "The Silvercat file not found among:"
		puts stderr $possible_agfiles
		puts stderr "Use -f <silvercat file> to set it explicitly"
		exit 1
	}
} else {
	if { ![file exists $agfile] } {
		puts stderr "File not found: $agfile"
		exit 1
	}
}

set arg1 [lindex $g_args 0]
if { $arg1 == "" } {
	set arg1 help
}

# XXX This should be somehow modified to support shadow builds
# 
set agv::srcdir [file dirname $agfile]
source $agfile

exit [ag-do-$arg1 [lrange $g_args 1 end]]


}
