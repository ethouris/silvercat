


proc pexpand {arg {ulevel 2}} {
	# This trick should expand the list in place and pack
	# it back to the list. This replaces all first-level whitespaces
	# into single spaces.
	set cmd "subst {{$arg}}"
	#puts stderr "EXPANDING BY: $cmd"
	set code [catch {list {*}[uplevel $ulevel [list eval $cmd]]} result]
	#puts stderr "EXPANDED: $result"
	if {$code} {
		puts stderr "*** ERROR: can't expand: $result"
		puts stderr "*** available variables: [uplevel $ulevel [list info vars]]"
		error "Expanding '$arg'"
	}

	return [lindex $result 0]
}

proc pset {name arg1 args} {
    upvar $name var

    set var [pexpand $arg1]
    foreach a $args {
		append var " [pexpand $a]"
    }
	return $var
}

proc RegexpJoinWords {a1 args} {
	set out "^($a1"
	foreach a $args {
		append out "|$a"
	}
	append out ")$"
	return $out
}

proc plremove {list item args} {
	if { $args == "" } {
		return [lsearch -not -all -inline -exact $list $item]
	}
	return [lsearch -not -all -inline -regexp $list [RegexpJoinWords $item {*}$args]]
}

proc pset+ {name arg1 args} {
    upvar $name var
    append var " [pexpand $arg1]"
    foreach a $args {
	append var " $a"
	 }
	return $var
}

proc pinit {name args} {
	upvar $name var
	if { [info exists var] } {
		return $var
	}

	uplevel pset $name {*}$args
	
	return $var
}

proc phas {name} {
    upvar $name lname
    if {![info exists lname]} {
	return 0
    }
    return [string is true $lname]
}

proc pget {name {default ""}} {

	# If part of the name is indexed array, then
	# shift to its end before looking for a dot.
	# That is, don't include the index value in dot search.

	set endpar [string first ")" $name]
	if { $endpar != -1 } {
		set before [string range $name 0 $endpar]
		set name [string range $name $endpar+1 end]
	} else {
		set before ""
	}

	if { [string first . $name] != -1 } {
		set path [lassign [split $name .] name]
	} else {
		set path ""
	}

	# Restore name that was taken part from
	if { $endpar != -1 } {
		set name "$before$name"
	}

    upvar $name lname
    if {![info exists lname]} {
		return $default
    }

	# If the name appeared to be a Tcl array, process it differently
	if { [array exists lname] } {
		if { $path != "" } {
			# Take the 'path' as a full index, even if it contains spaces.
			set apath [array names lname -exact $path]
			if { $apath == "" } {
				return $default
			}

			return $lname($path)
		} else {
			return [array get lname]
		}
	}

	# So, it appeared to be a value...
	if { $path != "" } {

		if { [llength $lname] % 2 == 1 } {
		        # This is an odd list, so cannot be a dict
		        return $default
		}

		if { ![dict exists $lname {*}$path] } {
		        return $default
		}
		return [dict get $lname {*}$path]
	}

    return $lname
}

proc palias {name body} {
	set procbody "return \[$body {*}\$args\]"
	namespace inscope :: proc $name args $procbody
}

proc pdef {name args} {
	if { [llength $args] == 1 } {
		set args [lindex $args 0]
	}
	namespace inscope :: proc $name {} "return \[uplevel concat $args\]"
}

proc pdefv {name args} {
	set e [catch {expr $args} result]
	if { $e } {
		puts stderr "NOTE: '$args' can't be evaluated: $result"
		set result $args
	} 
	namespace inscope :: proc $name {} [list return $result]
}

proc pdefx {name arg} {
	namespace inscope :: proc $name {} "return \[uplevel expr {$arg}\]"
}

proc pwrite {filename contents} {
	set fd [open $filename w]
	puts $fd $contents
	close $fd
}

proc pread {filename} {
	set fd [open $filename r]
	set con [read $fd]
	close $fd
	return $con
}

proc pupdate {filename contents} {
	if { [file exists $filename] } {
		set oldcon [pread $filename]
		if { [string trim $oldcon] == [string trim $contents] } {
		        return 0
		}
	}
	pwrite $filename $contents
	return 1
}

# This function is directly copied from 'apply' Tcl manpage.
# Just wanted to be clear about it, although it doesn't kick, but...
proc pmap {lambda list} {
	set result {}
	if { [llength $lambda] == 1 } {
		# Then it's a command name
		foreach item $list {
		        lappend result [$lambda $item]
		}
	} else {
		foreach item $list {
		        lappend result [apply $lambda $item]
		}
	}
	return $result
}

proc pfind {args} {
	# First, test if the last one is a list of directories or a mask
	set last [lindex $args end]
	if { [string first * $last] != -1 || [string first "\[" $last] != -1 || [string first ? $last] != -1 } {
		# This is a mask and it cannot be a directory.
		set masks $args
		set directories .
	} else {
		set masks [lrange $args 0 end-1]
		set directories [lindex $args end]
	}

	set outlist ""
	foreach d $directories {
		foreach m $masks {
		        if { $d == "." } {
		                set p $m
		        } else {
		                set p $d/$m
		        }
		        lappend outlist {*}[glob -nocomplain $p]
		}
	}

	return $outlist
}

proc psearch {files args} {
	set out ""

	foreach file $files {
		foreach dir $args {
		        lappend out {*}[glob -nocomplain [file join $dir $file]]
		}
	}

	return $out
}

proc prelocate {path {wd .} {top ""}} {

	if { $wd == "." } {
		set wd [pwd]
	} else {
		set wd [file normalize $wd]
	}

	set norm [file normalize $path]

	if { $norm == $wd } {
		return .
	}

	if { $top != "" } {
		# This means that we want the relative path only
		# up to given "toplevel directory". If the 'norm'
		# path is not path that leads down the toplevel
		# directory, return absolute path.

		if { [file pathtype $top] ni {absolute} } {
			set top [file normalize $top]
		}

		if { [string first $top $norm] != 0 } {
			# equal to 0 means that $norm starts exactly from $top
			# Here it's not, which means, return absolute path
			return $norm
		}
	}

	set common 0
	set norm_parts [file split $norm]
	set b_parts [file split $wd]
	set max [expr {max([llength $norm_parts],[llength $b_parts])}]
	#puts "prelativize: NORM: $norm_parts B: $b_parts -- looking for diffs up to $max"
	while { [lindex $norm_parts $common] == [lindex $b_parts $common] } {
		incr common
		if { $common == $max } {
		        break
		}
	}

	set shift_norm_parts [lrange $norm_parts $common end]
	set overhead [expr {[llength $b_parts]-$common}]
	set uppath ""
	#$mkv::debug "Adding up-dir overhead: $overhead"
	if { $overhead > 0 } {
		set uppath [lrepeat $overhead ..]
	}
	set rpath [file join {*}$uppath {*}$shift_norm_parts]

	if { $rpath == "" } {
		return .
	}

	#$mkv::debug "Norma-localize in '$wd' $norm: $rpath"
	return $rpath
}


proc number-cores {} {
	# This should return some system-dependent number
	# of cores. This should be done some system-dependent
	# way, so far we'll just use
	# - on Linux-compliant systems (including Cygwin), use /proc/cpuinfo
	# - on Darwin, use sysctl hw.ncpu
	# - on others, return 2.

	switch -glob -- $::tcl_platform(os) {
		CYGWIN* - Linux {
		        set nc [exec grep "^processor\[ \t\]:" /proc/cpuinfo | wc -l]
		}

		Darwin {
		        set nr [exec sysctl hw.ncpu]
		        set nc [string trim [lindex [split $nr :] 1]]
		}

		default {
		        set nc 2
		}
	}

	if { ![string is integer $nc] || $nc < 2 } {
		return 2
	}
	return $nc
}

proc pflat args {
	set target ""
	foreach el $args {
		append target "[string map {\n { }} $el] "
	}
	return $target
}

proc pluniq ls {
	set output ""

	foreach l $ls {
		if { $l ni $output } {
			lappend output $l
		}
	}
	return $output
}

proc pluappend {r_ls args} {
	upvar $r_ls ls
	if { ![info exists ls] } {
		set ls ""
	}
	foreach a $args {
		if { $a ni $ls } {
			lappend ls $a
		}
	}
	return $ls
}

proc puncomment text {
	set lines [split $text \n]
	set output {}
	foreach line $lines {
		set linetrimmed [string trimleft $line]
		if { [string index $linetrimmed 0] != "#" } {
		        lappend output $line
		}
	}
	return [join $output \n]
}

proc plist1 arg {
	set out ""
	set arg [string map {"\\\n" ""} $arg]

	 set lines [split $arg \n]
	 foreach l $lines {
		 #puts stderr "LINE: $l"
		 set lt [string trimleft $l]
		 if { [string index $lt 0] == "#" } {
		         continue
		 }

		# Escape the braces
		set l [string map {"\{" "\\\{" "\}" "\\\}" ";" "\\;"} $l]
		 set ol [uplevel "list $l"]
		 #puts stderr "OUTPUT LINE: $ol"
		 lappend out $ol
		 #puts stderr "STATE: $out"
	 }
 
	 #puts "RESULT: $out"
 
	 return [join $out \n]
}

proc plist args {
	if { [llength $args] == 1 } {
		return [uplevel [list [namespace current]::plist1 [lindex $args 0]]]
	}

	set out ""

	foreach a $args {
		lappend out [uplevel [list [namespace current]::plist1 $a]]
	}

	return $out
}

proc process-options {argv optargd} {

	array set optargs $optargd

	# Set first all boolean options to false
	foreach {on ov} $optargd {
		if { [string index $ov 0] == "*" } {
			set varname [string range $ov 1 end]
			upvar $varname r_$varname
			#puts stderr "OPTION VARIABLE: ::$varname (bool: 0)"
			if { ![info exists r_$varname] } {
				set r_$varname 0
			}
		} else {
			upvar $ov r_$ov
			#puts stderr "OPTION VARIABLE: ::$ov (string: '')"
			if { ![info exists r_$ov] } {
				set r_$ov ""
			}
		}
	}

	set args ""
	set variables ""

	set in_option ""

	foreach e $argv {

		set follow ""
		set continue 1
		# This is for the case when the whole body needs to be repeated
		# without getting to the next iteration
		while { $continue } {

			# This variable is set only if there are more than 0 optargs for this option
			# The in_option contains: <name of the option> <how many arguments to grab>
			if { $in_option != "" } {
				lassign $in_option on ox
				set os [llength $optargs($on)]
				set pos [expr {$os-$ox}]
				set varname [lindex $optargs($on) $pos]
				set r_$varname $e
				#puts stderr "OPTION: ::$varname = $e"
				incr ox -1
				if { $ox == 0 } {
					set in_option ""
				} else {
					set in_option [list $on $ox]
				}
				break ; # meaning, continue iterations
			}

			# Options start from - unless this is only -.
			if { [string index $e 0] == "-" && $e != "-" } {
				# Grab the next character:
				# - if this is "-", then take the whole word as option.
				# - otherwise, take a "one letter option"; if there are more
				# characters, treat it as its argument.
				if { [string index $e 1] != "-" } {

					# Option form -OARGUMENT (one dash, followed by other things)
					# MAKE e = "-O" follow = "ARGUMENT"

					set follow [string range $e 2 end]
					set e [string range $e 0 1]
				} elseif { [set pos [string first = $e 2]] != -1 } {

					# Otherwise we have --OPTION, check if you have --OPTION=ARGUMENT
					# If so, then make: e = "--OPTION" follow = "ARGUMENT"

					set follow [lrange $e $pos+1 end]
					set e [lrange $e 0 $pos-1]
				}
				
				#puts stderr "OPTION EXTRACTED: '$e' follow=$follow"

				if { [info exists optargs($e)] } {
					set oa $optargs($e)
					if { [string index $oa 0] == "*" } {
						# boolean option. set to 1,default is 0
						set varname [string range $oa 1 end]
						set r_$varname 1
						#puts stderr "OPTION: ::$varname = true (boolean)"

						if { $follow != "" } {
							# If there was any string following the boolean option,
							# make it look like another option.
							set e "-$follow"
							set follow ""
							#puts stderr "OPTION FOLLOWING: $e"
							continue ; # repeat current iteration
						}
						break ;# continue iterations
					}

					# Otherwise set the hook for the next iteration
					set in_option [list $e [llength $oa]]

					if { $follow != "" } {
						set e $follow
						set follow ""
						#puts stderr "OPTION REPEATED: $e"
						continue ;# repeat current iteration, don't get argument from the argument list
					}

					break ; # meaning, continue iterations
				}

				# This is for command-line so this is ok.
				error "No such option: $e"
			}

			set varval [split $e =]
			if { [llength $varval] == 1 } {
				lappend args $e
			} else {
				lappend variables [lindex $varval 0] [lindex $varval 1]
			}

			break
		}
	}

	return [list $args $variables]
}

proc pass args { return $args }

proc pver {args} {

	# We need to be prepared that it will be used as:
	# 1.2>2.3 or 1.2 >2.3 or 1.2 > 2.3.
	# So, simply treat all arguments as a text and extract all using one regexp.

	if { ![regexp {([0-9\.]+)\S*\s*([<>=]*)\s*([0-9\.]+)\S*} $args unu ver rel mver] } {
		error "pver: usage: <tested-version> \[relationship\] <template-version>, e.g. 1.3 >= 2.5.9"
	}

	set dist [package vcompare $ver $mver]
	if { $rel == "" } {
		set rel <=
	} elseif { $rel == "=" } {
		set rel ==
	}

	# Now we should have this <= >= == < >
	set fi [string index $rel 0]
	set ne [string index $rel 1]
	if { $fi ni {< > =} || $ne ni {= ""} } {
		error "pver: incorrect version relation expression: '$rel' - use <>= based less-greater-equal expression"
	}

	return [expr $dist $rel 0]
}

# Forwarder for standard-method running cmdline app
proc prun args {
	vlog "+run: $args"
	exec 2>@stderr >@stdout {*}$args
}


proc dict:assert dic {
	set llen [llength $dic]
	if { $llen%2 == 1 } {
		if { $llen > 80 } {
			set dic [string range $dic 0 80]...
		}
		error "input doesn't look like a dictionary: '$dic'"
	}
}

proc dict:at {dic args} {
	dict:assert $dic
	if { ![dict exists $dic {*}$args] } {
		return ""
	}

	return [dict get $dic {*}$args]
}

proc dict:sel {dic args} {
	dict:assert $dic

	set pipe [lsearch $args |]

	# Special case: set 'since' to 0 if both pipe was first
	# element or wasn't supplied.
	set since [expr {$pipe+1}]

	# If pipe was the first key, skip it.
	if { $since == 1 } {
		set since 0
		set args [lrange $args 1 end]
	}

	if { $since == 0 } {
		set input $dic
	} else {
		set input [dict get $dic {*}[lrange $args 0 $since-2]]
		set args [lrange $args $since end]
	}

	set output ""
	foreach x $args {
		if { $x == "\\|" } {
			set x |
		}
		lappend output [dict get $input $x]
	}
	return $output
}

proc dict:layout {layout list} {
	set output ""
	foreach k $layout v $list {
		dict set output $k $v
	}
	return $output
}

# The dict filter/key in Tcl 8.5 allows only ONE KEY pattern.
# We have to do do multiple times filtering.
# Only Tcl 8.6 allows multiple keys.
# Unfortunately the filtering is done by glob, and this doesn't allow alternatives.
if { $tcl_version < 8.6 } {
	proc dict:filterkey {dict args} {
		set out ""
		foreach k $args {
			lappend out {*}[dict filter $dict key $k]
		}
		return $out
	}
} else {
	proc dict:filterkey {dict args} {
		return [dict filter $dict key {*}$args]
	}
}


set public_export_util [puncomment {

	# Utility functions
	plremove
	pset
	pset+
	pinit
	pget
	phas
	palias
	pdef
	pdefv
	pdefx
	pwrite
	pread
	pupdate
	pexpand
	pfind
	psearch
	plist
	pmap
	prelocate
	puncomment
	pflat
	pluniq
	pluappend
	pass
	pver
	prun
	process-options
	number-cores
	dict:at
	dict:filterkey
	dict:sel
	dict:layout
}]

namespace export {*}$public_export_util

