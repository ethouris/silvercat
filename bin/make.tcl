#!/bin/bash
# but tcl \
exec tclsh8.5 "$0" "$@"

package require Tcl

# Debug
#package require Itcl

# Allow for version recognition by makefiles
package provide tclmake 0.5

variable g_depends
variable g_actions

variable g_first_rule {}

variable g_failed {}

variable g_action_performed 0

set g_shell {/bin/bash}
if { [info exists ::env(SHELL)] } { set g_shell $::env(SHELL) }

set g_action_performed 0

set g_debug_indent 0
proc debug_indent {} {
	return [string repeat \t $::g_debug_indent]
}

proc debug_indent+ {} {
	incr ::g_debug_indent
}

proc debug_indent- {} {
	incr ::g_debug_indent -1
}

proc debug str {
	puts stderr [debug_indent]$str
}

set g_debug pass

proc pass args { }

# Provide expansion in style of {expand} in tcl 8.5
# use any character provided in $ch
proc expandif {ch ls} {
	set final {}
	foreach e $ls {
		if { [string index $e 0] == $ch } {
		        foreach y [string range $e 1 end] {
		                lappend final $y
		        }
		} else {
		        lappend final $e
		}
	}
	return $final
}

proc rule args {
	set args [expandif @ $args]
	set size [llength $args]
	if { $size < 2 } {
		return -code error "+++ rule: at least two arguments required (name,action)"
	}

	set target [lindex $args 0]
	if { [llength $target] > 1 && [string index [lindex $target 0] end] == ":" } {
		set target [string map {"\\\n" ""} $target]
		set args $target
		set target [string range [lindex $target 0] 0 end-1]
	}
	set depends ""
	if { $size > 2 } {
		set depends {}
		foreach dep [lrange $args 1 end-1] {
		        append depends "$dep "
		}
	}

	set action [no_comment [lindex $args end]]

#   XXX Substitution defered to latest execution        
#        set action [uplevel #0 [list subst_action $action]]

	# Check if this is a generic rule (contains *)
	# remove all "* ? [ ]", but leave untouched "\* \? \[ \]"
	if { [check_generic_upd_first $target] } {
		$::g_debug "RULE '$target' is generic: {$action}"
		set ::g_generic($target) $action
	}
	
	$::g_debug "RULE '$target' DEPS: $depends"
	$::g_debug "RULE '$target' ACTION: {$action}"
	set ::g_depends($target) $depends
	set ::g_actions($target) $action

	vlog "Target $target = $::g_depends($target)"

	# This feature is blocked because specifying the target as "-name" conflicts with option recognotion
# --- 	# Make phony all targets with name starting from "-"
# --- 	foreach dep $depends {
# --- 		if { [string index $dep 0] == "-" } {
# --- 			$::g_debug "RULE '$target': dependency '$dep' is set phony"
# --- 			set ::g_phony($dep) ""
# --- 		}
# --- 	}
	set target
}

# This function checks if the target is generic and returns true if it is.
# If the target isn't generic, and it was first such a target in the session,
# it also sets its name as the default target.
proc check_generic_upd_first {target} {
	set checked [string map {{\*} {\*} {\?} {\?} {\[} {\[} {\]} {\]} * {} ? {} {[} {} {]} {}} $target]
	if { $checked != $target } {
		return true
	} else {
		if { $::g_first_rule == "" } {
			set ::g_first_rule $target
		}
	}
	return false
}

proc phony {name args} {
	check_generic_upd_first $name

	# Don't set depends, if already set (was defined already).
	# In this case, just set it phony flag.
	$::g_debug "PHONY: set to target '$name', deps: $args"
	if { ![info exists ::g_depends($name)] } {
    	set ::g_depends($name) $args
	}
	set ::g_phony($name) ""
}

proc flatten args {
	set target ""
	foreach el $args {
		append target "[string map {\n { }} $el] "
	}
	return $target
}

proc no_comment text {
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

proc rules {rulelist action} {
	set rulelist [no_comment $rulelist]
	set firstrule [lindex $rulelist 0]
	set rules [lrange $rulelist 1 end]

	eval "[flatten rule $firstrule] {$action}"
	foreach rule $rules {
		set link [list !link [lindex $firstrule 0]]
		eval "[flatten rule $rule] {$link}"
	}
}

proc subst_action action {
	set lines [split $action \n]
	set lines_target {}
	foreach action $lines {
		set action [string trim $action]
		if { $action == "" } continue

		set target {}
		foreach element $action {
			append target "$element "
		}
		lappend lines_target $target
	}

	set target [uplevel #0 subst [list $lines_target]]
	return $target
}

proc is_target_stale {target depend} {
	# Check if $depend is phony.
	# If phony, then just forward the request to its depends.
	# XXX THIS FEATURE IS SLIGHTLY CONTROVERSIAL.
	# Probably another type of target "forward" should exist, parallel to "phony".
	if { [info exists ::g_phony($depend)] } {
		set out false
		if { [info exists ::g_depends($depend)] && [expr {$::g_depends($depend) != ""}] } {
			vlog "... ... and has deps ..."
			foreach d $::g_depends($depend) {
				vlog "... ... forwarding: against $d ... [expr {[info exists ::g_phony($d)] ? "(also phony)" : ""}]"
				set stale [is_target_stale $target $d]
				set out [expr {$out || $stale} ]
				vlog "... stale: $out"
			}
		} else {
			vlog "... ... and has no deps ..."
			return true ;# phony that has no deps means always stale
		}
		return $out
	}
	if { [catch {set stale [expr {![file exists $target]
		        || [file mtime $depend] > [file mtime $target]} ]}] } {
		puts stderr "MTIME checked on nonexistent '$depend'"
		return false
	}

	return $stale
}

proc depends {args} {
	if { $args == {} } { set args [array names ::g_depends] }
	
	foreach dep $args {
		puts "$dep: $::g_depends($dep)"
	}
}

# - class Guard {
# -         variable varname
# -         constructor {var} {
# -                 set varname $var
# -                 incr $varname
# -         }
# - 
# -         destructor {
# -                 incr $varname -1
# -         }
# - }

variable g_depth 0
variable g_verbose 0

proc vlog text {
	if { !$::g_verbose } return

	set head ""

	for {set i 0} {$i < $::g_depth} {incr i} {
		append head "* "
	}

	puts stderr "$head $text"
}

proc make target {
	
	vlog "--- make $target ---"

	# Deny making targets, which already failed
	if { [lsearch $::g_failed $target] != -1 } {
		vlog "Target $target denied, because already failed"
		error "Derived failure from $target"
	}

	set status 1
	set hasdepends [info exists ::g_depends($target)] 

    if { !$hasdepends } {
		vlog "No direct depends, checking for generic depends"
        set generic [find_generic $target]
        if { $generic != "" } {
            set depends [generate_depends $target $generic $::g_depends($generic)]
            if { $depends != "" } {
                set hasdepends 1
            }
        }
    } else {
        set depends $::g_depends($target)
    }

	set result "(reason unknown)"
    if { $hasdepends } {
		vlog "Has dependencies: $depends"
        foreach depend $depends {
            vlog "Considering $depend as dependency for $target"
			if { [catch {make $depend} result] } {
                set status 0
                vlog "Making $depend failed, so $target won't be made"
				if { $::g_keep_going } {
					vlog "- although continuing with other targets (-k)"
				} else {
					break
				}
            }
        }
    }

 	if { $status == 0 } {
 		error "+++ Make failed for '$depend':\n$result"
 	}

	# If a phony target doesn't have action, it won't be checked for generic action, too.
	if { [info exists ::g_phony($target)] && ![info exists ::g_actions($target)] } {
		vlog "Target '$target' is phony and has no action - skipping"
		return
	}

	set stale 0
	
	set need_build 0
	set reason "is wrong"
	if { [info exists ::g_phony($target)] } {
		set need_build 1
		set reason "is phony"
	} elseif { ![file exists $target] } {
		set need_build 1
		set reason "is missing"
	}

	if { $need_build } {
		vlog "File '$target' $reason - performing action for '$target'"
		if { ![perform_action $target $target] } {
            error "Action failure for '$target'"
        }
	} elseif { $hasdepends } {
		vlog "Checking if $target is fresh:"
		foreach depend $depends {
            vlog "... against $depend [expr {[info exists ::g_phony($depend)] ? "(PHONY)":""}]..."
		    set stale [is_target_stale $target $depend]
			if { $stale } {
		        vlog "File '$target' is stale against '$depend', will be made"
		        break
		    }
		}

		if { $stale } {
			vlog "Performing make action for $target"
			if {![perform_action $target $target]} {
				error "Action failure for $target"
			}
		} else {
			vlog "File $target is fresh, so won't be made"
		}
	} else {
		vlog "File $target exists and has no dependencies, so won't be made"
	}
}

proc apply_action_options action {
	# reset options
	set ::g_quiet 0
	set ::g_ignore 0
	set ::g_escaped 0
	
	while 1 {
		set first [string index $action 0]
		if { $first == "@" } {
		        set ::g_quiet 1
		} elseif { $first == "-" } {
		        set ::g_ignore 1
		} elseif { $first == "!" } {
		        set ::g_escaped 1
		} else break
		set action [string range $action 1 end]
	}
	return $action
}

proc fresher_depends {target depends} {
	if { ![file exists $target] } {
		return $depends
	}
	set out ""
	foreach d $depends {
		if { [is_target_stale $target $d] } {
			lappend out $d
		}
	}

	return $out
}

proc apply_special_variables {action target} {
	set depends $::g_depends($target)

	set str [string map \
		                 [list \
		                        {$@} $target \
		                        {$<} [lindex $depends 0] \
		                        {$^} $depends \
								{$?} [fresher_depends $target $depends]] \
		                 $action]
	return $str
}

proc find_generic target {
	if { ![info exists ::g_generic] } return

	foreach rule [array names ::g_generic] {
		# Special case for rule == "*" to
		# prevent infinite loop of something
		# that can match everything in infinity
		if { [string index $rule end] == "*" } {
			set prefix [string range $rule 0 end-1]
			if { $prefix != "" } {
				if { [string first $prefix $target] != 0 } {
					# Prefix doesn't match anyway.
					return 
				} else {
					set target [string range [string length $prefix] end]
				}
			}

			# It is allowed to do:
			# rule first.second.* first.second.*.cc { ... }
			# The above procedure should cut off the 'first.second.' fragment
			# as a prefix and do check only on rest of the file. If the checked
			# target is, for example, 'first.second.none', then 'none' is the
			# effectively checked target name - and it matches in this case.

			# Check if 'target' has some dotted suffix
			# If it has, it means it doesn't match.
			if { [file extension $target] != "" } {
				return
			}
		}
		if { [string match $rule $target] } {
		        return $rule
		}
	}
}

proc match2regexp t {
	set t [string map {{\*} {\*} {\?} {\?} * {.*} ? . . {\.}} $t]
	# replace first found .* or . with (.*) or (.)

	set from 0
	set size [string length $t]
	while 1 {
		set first [string first . $t $from]
		if { $first == -1 } {
		        return $t
		}

		if { $first != 0 } {
		        if { [string index $t [expr $first-1]] == "\\" } {
		                set from [expr $first+1]
		                continue
		        }
		}
		set begin [string range $t 0 [expr $first-1]]
		set end 0
		set star ""
		if { [string index $t [expr $first+1]] == "*" } {
		        set end [expr $first+2]
		        set star "*"
		} else {
		        set end [expr $first+1]
		}
		set end [string range $t $end end]
		set repl "(.$star)"
		return "$begin$repl$end"
	}
}

proc generate_depends {target template depends} {
	# I think the only way to go is to translate glob-type to regexp-type :)
	# Replace only FIRST found!

	set template [match2regexp $template]

	regexp $template $target match first
	if { $first == "" } {
		return $depends
	}

	set result {}
	
	foreach dep $depends {
		lappend result [regsub {(.*)([?*])(.*)} $dep "\\1$first\\3"]
	}
	return $result
}

# Two arguments mean:
# target - name of target which identifies actions to be taken
# actual_target - name of target, which is actually being made

# If action is performed normally, both targets are the same
# If action is taken as a link to another target, second argument
# is the target actually built; first is only the indetifier of
# the action, which should be taken.
proc perform_action {target actual_target} {

	set sake ""
	if { $target != $actual_target } {
		set sake " for the sake of '$actual_target'"
	}

    vlog "Performing action defined for '$target'$sake"
	set generic ""
	
	if { ![info exists ::g_actions($target)] } {
        vlog "No specific actions found for '$target' - checking generic actions"
		set generic [find_generic $actual_target]
		if { $generic == "" } {
			puts stderr "+++ No rule to make target '$target'"
            lappend ::g_failed $target
            return false
		} else {
            vlog "Found generic '$generic' applicable for target '$target'"
        }
	}

	set actions ""
	set depends ""
	if { $generic == "" } {
		vlog "Performing action:\n>>> $::g_actions($target)"
		set depends $::g_depends($actual_target)
		set actions [apply_special_variables $::g_actions($target) $actual_target]
	} else {
		vlog "Performing action:\n>>> $::g_actions($generic)"
		set depends [generate_depends $actual_target $generic $::g_depends($generic)]

		# Update dependencies for generated generic target
		set ::g_depends($actual_target) $depends
		set actions [apply_special_variables $::g_actions($generic) $actual_target]
	}

	# XXX Action variable substitution replaced only here!
	set actions [uplevel #0 [list subst_action $actions]]

	#Set special values

	foreach action $actions {
        set ::g_action_performed 1
		# apply standard make options

		set action [apply_action_options [flatten $action]]

		if { $::g_escaped } {
			set command [lindex $action 0]
			set arglist [lrange $action 1 end]

			switch -- $command {
				link {
					if { ![info exists ::g_actions($arglist)] } {
						puts stderr \
						     "+++ Can't resolve $arglist as a link to action"
						lappend ::g_failed $target
						return false
					}

					# Do substitution before altering target
					if { ![perform_action $arglist $target] } {
						lappend ::g_failed $target
						return false
					}
					continue
				}

				tcl {
					if { !$::g_quiet } {
						puts "CUSTOM COMMAND: $arglist"
					}
					uplevel #0 [list eval $arglist]
					continue
				}
			}

		}

		if { !$::g_quiet } {
			puts $action
		}

		#set waserr [catch {exec $::g_shell -c $action} result]; set retcode $::errorCode
		set waserr [catch {exec $::g_shell -c $action 2>@stderr >@stdout} result]; set retcode [pget ::errorCode]
		set failed [expr {$retcode != "NONE"}]
		if { $waserr } {
		        if { $result != "" } {
		                puts stderr $result
		        }
		        if { $failed } {
                   if { !$::g_ignore } {
		                puts stderr "+++ Action for '$actual_target' failed!"
		                lappend ::g_failed $target
		                return false
                   } else {
                       puts stderr "+++ Action for '$actual_target' failed (but ignored)."
                   }
		        }
		} else {
		        if { $result != "" } {
		                puts $result
		        }
		}
	}

	return true
}

# Autoclean automaton

proc rolling_autoclean {rule debug} {

	set autoclean_candidates ""
	$debug "TO CLEAN $rule:"
	set deps ""
	if { [info exists ::g_depends($rule)] } { lappend deps {*}$::g_depends($rule) }
	if { [info exists ::g_generic($rule)] } { lappend deps {*}$::g_generic($rule) }
	set generic [find_generic $rule]
	if { $generic != "" } {
		lappend deps {*}[generate_depends $rule $generic $::g_depends($generic)]
	}

	if { $deps == "" } {
		$debug "WON'T DELETE: $rule - no dependencies (generics: $generic)"
	} else {
		if { ![info exists ::g_actions($rule)] && $generic == "" } {
			$debug "WON'T DELETE: $rule - no action (phony rule)"
		} else {
			$debug "WILL DELETE: $rule (built from $deps)"
			lappend autoclean_candidates $rule
		}
	}

	incr ::g_debug_indent
	foreach dep $deps {
		lappend autoclean_candidates {*}[rolling_autoclean $dep $debug]
	}
	incr ::g_debug_indent -1
	return $autoclean_candidates
}

proc autoclean {rule} {
	set ::g_debug_indent 0
	set ac [rolling_autoclean $rule pass]
	if { $ac != "" } {
		puts stderr "Autoclean deletes: $ac"
		file delete {*}$ac
	}
}

proc autoclean-test {rule} {
	set ::g_debug_indent 0
	set ac [rolling_autoclean $rule debug]
	puts stderr "Autoclean would delete: $ac"
}

proc pexpand {arg {ulevel 2}} {
	# This trick should expand the list in place and pack
	# it back to the list. This replaces all first-level whitespaces
	# into single spaces.
	set code [catch {list {*}[uplevel $ulevel [list subst $arg]]} result]
	if {$code} {
		puts stderr "*** ERROR: can't expand: $result"
		puts stderr "*** available variables: [uplevel $ulevel [list info vars]]"
		error "Expanding '$arg'"
	}

	return $result
}

proc pset {name arg1 args} {
    upvar $name var

    set var [pexpand $arg1]
    foreach a $args {
        append var " $a"
    }
}

proc pset+ {name arg1 args} {
    upvar $name var
    append var " [pexpand $arg1]"
    foreach a $args {
        append var " $a"
    }
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

proc pdef {name args} {
	proc $name {} "return {$args}"
}

proc pdefx {name args} {
	set e [catch {expr $args} result]
	if { $e } {
		puts stderr "NOTE: '$args' can't be evaluated: $result"
		set result $args
	} 
	proc $name {} [list return $result]
}


# utilities (for debug stuff)

proc tribool_human in {
	if { $in == 0 } {
		return no
	} elseif { $in == 1 } {
		return yes
	}
	return maybe
}

proc tribool_logical in {
	if { $in == 0 } {
		return false
	} elseif { $in == 1 } {
		return true
	}
	return indeterminate
}

proc mk-process-options {argv optargd} {

	array set optargs $optargd

	set args ""
	set variables ""

	set in_option ""

	foreach e $argv {

		# This variable is set only if there are more than 0 optargs for this option
		if { $in_option != "" } {
			lassign $in_option on ox
			set os [llength $optargs($on)]
			set pos [expr {$os-$ox}]
			set varname [lindex $optargs($on) $pos]
			upvar $varname r_$varname
			set r_$varname $e
			incr ox -1
			if { $ox == 0 } {
				set in_option ""
			} else {
				set in_option [list $on $ox]
			}
			continue
		}

		if { [string index $e 0] == "-" } {
			if { [info exists optargs($e)] } {
				set oa $optargs($e)
				if { [string index $oa 0] == "*" } {
					# boolean option. set to 1,default is 0
					set varname [string range $oa 1 end]
					upvar $varname r_$varname
					set r_$varname 1
					continue
				}

				# Otherwise set the hook for the next iteration
				set in_option [list $e 1]
				continue
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
	}

	return [list $args $variables]
}

set g_keep_going 0
set g_debug_on 0

package provide make 1.0

# Rest of the file is interactive.
if { !$tcl_interactive } {

set makefile {}
set help 0

set g_optargs {
	-k *g_keep_going
	-f makefile
	--help *help
	-help *help
	-d *g_debug_on
	-v *g_verbose
}

lassign [mk-process-options $argv $g_optargs] g_args g_variables

if { $help } {
	puts stderr "Options:"
	foreach {opt arg} $g_optargs {
		puts stderr [format "  %-8s %s" $opt: $arg]
	}
	exit 1
}

if { $g_debug_on } {
	set g_debug debug
}

foreach {name value} $g_variables {
    #puts "Setting $name to $value"
    set $name $value
}

if { $makefile == "" } {
	if { [file exists Makefile.tcl] } {
		set makefile Makefile.tcl
	} elseif { [file exists makefile.tcl] } {
		set makefile makefile.tcl
	} else {
		error "Makefile.tcl not found"
	}
} elseif { ![file exists $makefile] } {
	error "Makefile '$makefile' not found"
}


source $makefile

if {$g_args == ""} {set g_args $g_first_rule}

if { [set xc [catch {
        foreach tar $g_args {
            make $tar
        }
    } result]] } {

		puts stderr $result
		puts stderr "+++ Target '$tar' failed"
		#puts stderr "+++ $errorInfo"
		exit 1
}

if { !$g_action_performed } {
    puts stderr "+++ Nothing to be done for: $g_args"
}

} ;# end of interactive actions
