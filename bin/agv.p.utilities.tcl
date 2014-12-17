#!/bin/echo "This should be only used internally by ag.tcl. Don\'t use it manually."

# Before adding anything here, mind that you are here in the agv::p namespace!

proc lsuniq {ls} {
	set out ""
	foreach e $ls {
		if { $e ni $out } {
			lappend out $e
		}
	}

	return $out
}

namespace export lsuniq

proc dict:at {dic args} {
	if { [llength $dic]%2 == 1 } {
		error "This doesn't look like a dictionary: '$dic'"
	}
	if { ![dict exists $dic {*}$args] } {
		return ""
	}

	return [dict get $dic {*}$args]
}

namespace export dict:at

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
		for {set i 0} {$i < $point} {incr i} {
			foreach s $targets {
				if { $s in [lindex $targets $i] } {
					set msg "Cyclic dependency in definition of taret '$s' ($i)"
					error $msg
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

proc PrepareGeneralTarget {} {
	# This procedure should collect all targets of
	# type 'program' or 'library' and create a phony
	# target named 'all' that has them all as dependencies

	set subtargets {}
	# Extract only targets of type program or library
	foreach t [array names agv::target] {
		set type [dict:at $agv::target($t) type]
		vlog "Target '$t', type $type:"
		if { $type in {program library custom} } {
			vlog " --> Added to 'all' (because $type)"
			lappend subtargets $t
		} elseif { $type == "directory" } {
			vlog " --> Added to 'all' as $t/all (because $type)"
			lappend subtargets $t/all
		} else {
			vlog " --| Not added to 'all' (because $type)"
		}
	}

	vlog " --: ALL TARGETS: $subtargets"
	set subtargets [ReorderTargets $subtargets]

	vlog " --: ALL TARGETS: $subtargets"
	ag all -type phony -depends {*}$subtargets
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

	set languages [lsuniq [concat $languages $extralangs]]

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

