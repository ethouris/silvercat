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

