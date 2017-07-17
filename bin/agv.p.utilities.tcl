#!/bin/echo "This should be only used internally by ag.tcl. Don\'t use it manually."

# Before adding anything here, mind that you are here in the agv::p namespace!

proc GenFileBase {method target srcfile} {
	return [gen-imfile-$method $target $srcfile]
}

namespace export GenFileBase

proc gen-imfile-path {target srcfile} {
	if { [string index $srcfile 0] == "/" } {
		set pos 1
		while {[string index $srcfile $pos] == "/" } {
			incr pos
		}
		set srcfile "_-[string range $srcfile $pos end]"
	}
	return [string map {../ _ ./ {} / -} [file rootname $srcfile]]
}

proc gen-imfile-name {target srcfile} {
	return [file rootname [file tail $srcfile]]
}

proc gen-imfile-target-path {target srcfile} {
	return $target-[gen-imfile-path $target $srcfile]
}

proc gen-imfile-target-name {target srcfile} {
	return $target-[file rootname [file tail $srcfile]]
}

namespace export {
	gen-imfile-path
	gen-imfile-name
	gen-imfile-target-path
	gen-imfile-target-name
}



# XXX
# NOTE: This code comes from rosettacode.org and can be found here:
# http://rosettacode.org/wiki/Topological_sort#Tcl
# I state it's distributed under GNU Free Documentation License 1.2, as stated in the title page.
proc topsort {data} {
	# Clean the data
	dict for {node depends} $data {
		if {[set i [lsearch -exact $depends $node]] >= 0} {
			set depends [lreplace $depends $i $i]
			dict set data $node $depends
		}
		foreach node $depends {dict lappend data $node}
	}
	# Do the sort
	set sorted {}
	while 1 {
		# Find available nodes
		set avail [dict keys [dict filter $data value {}]]
		if {![llength $avail]} {
			if {[dict size $data]} {
				error "graph is cyclic, possibly involving nodes \"[dict keys $data]\""
			}
			return $sorted
		}
		# Note that the lsort is only necessary for making the results more like other langs
		lappend sorted {*}[lsort $avail]
		# Remove from working copy of graph
		dict for {node depends} $data {
			foreach n $avail {
				if {[set i [lsearch -exact $depends $n]] >= 0} {
					set depends [lreplace $depends $i $i]
					dict set data $node $depends
				}
			}
		}
		foreach node $avail {
			dict unset data $node
		}
	}
}

namespace export topsort

proc ReorderTargets subtargets {
	set ltargets {}
	lappend ltargets {*}$subtargets
	set point 0
	set nomore true
	set targets {}
	set nest 50
	set children() ""

	while 1 {

		if { ![incr nest -1] } {
			error "Building dependency graph reached nest >50 - crashing to prevent infinite loop"
		}

		foreach t [lindex $ltargets $point] {
			set deps [dict:at $agv::target($t) depends]
			lappend targets {*}$deps
			lappend children($t) {*}$deps
			vlog "{$point} DEPS OF $t: $targets"
		}

		if { $targets == "" } {
			break
		}

		# Check if any of found target doesn't already occur in previous ones
		$::g_debug "CHECKING CYCLE: '$targets' in <0,$point>[lrange $ltargets 0 $point-1]"
		for {set i 0} {$i < $point} {incr i} {
			foreach s $targets {
				if { $s in [lindex $ltargets $i] } {
					#set msg "Cyclic dependency in definition of taret '$s' ($i)"
					#error $msg
					vlog "NOTE: Dropping circular dependency in $s (as [lindex $ltargets $i] from $t)"
					set i $point ;# force break outer for
					set targets "" ;# prevent adding
					break
				}
			}
		}

		lappend $ltargets {*}$targets
		set targets ""
		incr point
		vlog "LTARGETS:"
		set i 0
		foreach lt $ltargets {
			vlog " -- {$i} $lt"
			incr i
		}
	}


	if { $ltargets == "" } {
		vlog "NO target dependency tree (only first level deps): $subtargets"
		return $subtargets
	}

	set kidless ""

	# Build dependency tree
	set tree {}
	foreach {node kids} [array get children] {

		if { $kids != "" } {
			lappend tree $node $kids
		}
	}

	vlog "Target dependency tree: $tree (subs: $subtargets)"

	set tree [topsort $tree]

	foreach et $subtargets {
		if { $et ni $tree } {
			vlog " ... adding childless dep: $et"
			lappend tree $et
		}
	}
	

	return $tree
}

proc PrepareGeneralTargets {} {
	# This procedure should collect all targets of
	# type 'program' or 'library' and create a phony
	# target named 'all' that has them all as dependencies

	set subtargets {}
	# Extract only targets of type program or library
	foreach t [array names agv::target] {
		set type [dict:at $agv::target($t) type]
		set runon [dict:at $agv::target($t) runon]
		set addtargets ""
		$::g_debug "Target '$t', type $type, runon $runon:"

		if { $type == {directory} } {
			# Directory must be treated special way
			$::g_debug " --> Added to 'all' as $t/all (because $type)"
			CheckDefinedTarget $t/all  ;# Create it lazily
			set addtargets "$t $t/all"
		} elseif { {all} in [pget agv::p::typeflags($type)] } {
			$::g_debug " --> Added to 'all' (because $type)"
			set addtargets $t
		} else {
			$::g_debug " --| Not added to 'all' (because $type)"
		}

		if { $addtargets != "" } {
			lappend subtargets {*}$addtargets
		}
	}

	vlog " --: TOTAL EXTRACTED TARGETS: $subtargets"
	set subtargets [ReorderTargets $subtargets]

	set otargets ""
	# Now filter out those targets that are already
	# dependencies on other targets
	foreach t $subtargets {
		set any 0
		vlog " -- CHECKING if '$t' is top-level target"
		foreach d $subtargets {
			if { $t == $d } {
				continue
			}
			set tdep [pget agv::target($d).depends]
			vlog " -- CHECKING $d's depends: $tdep"
			if {$t in $tdep} {
				set any 1
				vlog " -- REMOVED $t because it's a dependency of $d"
				break
			}
		}
		if { !$any } {
			vlog " -- TARGET $t is top-level - adding to all"
			lappend otargets $t
		}
	}
	set subtargets $otargets

	# The "reconfigure" target is somewhat special and MUST BE FIRST, always.
	# If found, move it to the first one.
	if { "reconfigure-ifneeded" in $subtargets } {
		set subtargets [concat reconfigure-ifneeded [plremove $subtargets reconfigure-ifneeded]]
	}

	# Ok, now construct alltargets that consist only of targets
	# that are not set -runon demand.

	set alltargets ""
	set extratargets ""
	foreach t $subtargets {
		set runon [dict:at $agv::target($t) runon]
		if { $runon != "demand" } {
			lappend alltargets $t
		} else {
			lappend extratargets $t
		}
	}

	$::g_debug " --: TOTAL TARGETS: $subtargets"
	$::g_debug " --: ALL TARGETS: $alltargets"
	ag all -type phony -depends {*}$alltargets
	ag . -type phony -depends all {*}$extratargets

	# Add installation targets for all targets that have installation rules
	# Non-installable targets (category: noinst) should be excluded

	set installdeps ""
	foreach t $alltargets {
		set ic [ag $t ?install]
		if {  $ic != "" && $ic != "noinst" } {
			lappend installdeps "install-$t"
		}
	}

	$::g_debug " ... Generating install with: $installdeps"
	if { $installdeps != "" } {
		dict set phony install $installdeps
		dict set agv::target(all) phony $phony
	}
}

proc GetHeaderSuffixes {languages} {

	variable [namespace current]::compatible_langs

	set hsufs ""
	set extralangs ""

	foreach l $languages {
		if { [dict exists $compatible_langs $l] } {
			lappend extralangs {*}[dict get $compatible_langs $l]
		}
	}

	set languages [pluniq [concat $languages $extralangs]]

	foreach l $languages {
		set sufl [dict:at $agv::p::langextmap $l-header]
		if { $sufl != "" } {
			lappend hsufs {*}$sufl
		}
	}

	return $hsufs
}

proc GetCommonLanguage langs {

	variable [namespace current]::compatible_langs

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

			set c [dict:at $compatible_langs $a]
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

