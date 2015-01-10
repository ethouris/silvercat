#!/bin/bash
# but tcl \
exec tclsh8.5 "$0" "$@"

package require Tcl

# Debug
#package require Itcl

namespace eval mkv {

set debug mkv::p::pass


namespace eval p {

# Forwarder for standard-method running cmdline app
proc run args {
	puts stderr "+run: $args"
	exec 2>@stderr >@stdout {*}$args
}

variable db_depends
variable db_actions
variable db_generic
variable db_phony

# structure {actual_target target depparent}
variable q_pending

# structure {target status}, where status is
# 0 - succeeded (dependent targets can be started)
# 1 - failed first
# 2 - dropped because dependent target failed
variable q_done


variable first_rule {}

variable failed {}

variable action_performed 0

set shell {/bin/bash}
if { [info exists ::env(SHELL)] } { set shell $::env(SHELL) }

variable gg_debug_indent 0

variable keep_going 0


proc debug_indent {} {
	variable gg_debug_indent
	return [string repeat \t $gg_debug_indent]
}

proc debug_indent+ {} {
	variable gg_debug_indent
	incr gg_debug_indent
}

proc debug_indent- {} {
	variable gg_debug_indent
	incr gg_debug_indent -1
}

proc debug str {
	puts stderr [debug_indent]$str
}

proc pass args { return $args }


#namespace export debug
#namespace export debug_indent+
#namespace export debug_indent-

# state of actions
variable escaped
variable quiet
variable ignore

proc ppspawn {args} {
	set res ""

	# This is lamah, but I really don't have much choice :)
	set tracername "::S"

    foreach cmd $args {
        set cc [open "|$cmd 2>@stderr"] 
        dict set res $cc command $cmd
        fconfigure $cc -blocking 0 -buffering line
		# cut off the "file" prefix, should it be any other word
		set id [lindex [regexp -inline {[a-zA-Z]*([0-9]+)} $cc] 1]
		if { $id == "" } {
			set id $cc
		}
		append tracername ".$id"
    }

	set all [dict keys $res]
	foreach cc $all {
		fileevent $cc readable "set $tracername $cc"
	}
	dict set res tracer $tracername

	return $res
}

proc pptrace {res {vblank {}}} {

	set tracername [dict get $res tracer]
	set res [dict remove $res tracer]
	set background [dict keys $res]

    while 1 {
        foreach cc $background {
            if {[eof $cc]} {
                if {[set idx [lsearch -exact $background $cc]] >= 0} {
                    set background [lreplace $background $idx $idx]
                }
                catch [close $cc] cres copts
                dict set res $cc result $cres
                dict set res $cc options $copts
            } else {
				# Roll until EOF or EAGAIN.
				# When EAGAIN, it will be retried at the next roll.
				# When EOF, it will be removed from the list at the next roll.
				while { [gets $cc linein] != -1 } {
                	puts "\[$cc\] $linein"
				}
            }
        }

        if {[llength $background] == 0} {
			# All processes finished.
			break
        }
		if { $vblank != "" } {
			apply $vblank $background
		}

		# Ok, now clear the variable
		set $tracername ""
		vwait $tracername
		#puts "UNBLOCKED BY: [set $tracername]"
    }
    return $res
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

		# This variable is set only if there are more than 0 optargs for this option
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
			continue
		}

		if { [string index $e 0] == "-" } {
			if { [info exists optargs($e)] } {
				set oa $optargs($e)
				if { [string index $oa 0] == "*" } {
					# boolean option. set to 1,default is 0
					set varname [string range $oa 1 end]
					set r_$varname 1
					#puts stderr "OPTION: ::$varname = $e"
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

# Provide expansion in style of {*} in tcl 8.5
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
	variable db_depends
	variable db_generic
	variable db_phony
	variable db_actions
	variable db_prereq
	
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
	set prereq ""
	set depvar depends
	if { $size > 2 } {
		set depends {}
		foreach dep [lrange $args 1 end-1] {
			if { $dep == "|" } {
				set depvar prereq
			} else {
				append $depvar "$dep "
			}
		}
	}

	set action [puncomment [lindex $args end]]

	# Check if this is a generic rule (contains *)
	# remove all "* ? [ ]", but leave untouched "\* \? \[ \]"
	if { [check_generic_upd_first $target] } {
		$mkv::debug "RULE '$target' is generic: {$action}"
		set db_generic($target) $action
	}
	
	$mkv::debug "RULE '$target' DEPS: $depends PREREQ: $prereq"
	$mkv::debug "RULE '$target' ACTION: {$action}"
	set db_depends($target) $depends
	set db_actions($target) $action
	set db_prereq($target) $prereq

	vlog "Target $target = $db_depends($target) | $db_prereq($target)"

	set target
}

# This function checks if the target is generic and returns true if it is.
# If the target isn't generic, and it was first such a target in the session,
# it also sets its name as the default target.
proc check_generic_upd_first {target} {
	variable first_rule
	set checked [string map {{\*} {\*} {\?} {\?} {\[} {\[} {\]} {\]} * {} ? {} {[} {} {]} {}} $target]
	if { $checked != $target } {
		return true
	} else {
		if { $first_rule == "" } {
			set first_rule $target
		}
	}
	return false
}



proc phony {name args} {
	variable db_depends
	variable db_phony

	check_generic_upd_first $name

	# Don't set depends, if already set (was defined already).
	# In this case, just set it phony flag.
	$mkv::debug "PHONY: set to target '$name', deps: $args"
	if { ![info exists db_depends($name)] } {
    	set db_depends($name) $args
	}
	set db_phony($name) ""
}

proc flatten args {
	set target ""
	foreach el $args {
		append target "[string map {\n { }} $el] "
	}
	return $target
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

proc rules {rulelist action} {
	set rulelist [puncomment $rulelist]
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

	vlog "Substituting: $lines_target"

	set o [catch {
	set target [uplevel #0 subst [list $lines_target]]
	}]
	if { $o } {
		vlog ERROR:$::errorInfo
	}
	vlog "Substituted: ---> $target"

	return $target
}

proc is_target_stale {target depend} {
	# Check if $depend is phony.
	# If phony, then just forward the request to its depends.
	# XXX THIS FEATURE IS SLIGHTLY CONTROVERSIAL.
	# Probably another type of target "forward" should exist, parallel to "phony".
	if { [info exists mkv::p::db_phony($depend)] } {
		set out false
		if { [info exists db_depends($depend)] && [expr {$db_depends($depend) != ""}] } {
			vlog "... ... and has deps ..."
			foreach d $db_depends($depend) {
				vlog "... ... forwarding: against $d ... [expr {[info exists mkv::p::db_phony($d)] ? "(also phony)" : ""}]"
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
	if { $args == {} } { set args [array names db_depends] }
	
	foreach dep $args {
		puts "$dep: $db_depends($dep)"
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

variable depth 0
variable verbose 0

proc vlog text {
	variable verbose
	variable depth

	if { !$verbose } return

	set head ""

	for {set i 0} {$i < $depth} {incr i} {
		append head "* "
	}

	puts stderr "$head $text"
}

proc build_make_tree {target depparent} {

	variable db_depends
	variable db_generic
	variable db_phony
	variable db_actions
	variable db_prereq
	
	vlog "--- make $target ---"

	# Deny making targets, which already failed
	if { [lsearch $mkv::p::failed $target] != -1 } {
		vlog "Target $target denied, because already failed"
		error "Derived failure from $target"
	}

	set status 1
	set hasdepends [info exists db_depends($target)] 
	set prereq ""

    if { !$hasdepends } {
		vlog "No direct depends, checking for generic depends"
        set generic [find_generic $target]
        if { $generic != "" } {
            set depends [generate_depends $target $generic $db_depends($generic)]
            if { $depends != "" } {
                set hasdepends 1
            }
			if { [info exists db_prereq($generic)] } {
				set prereq [generate_depends $target $generic $db_prereq($generic)]
			}
			vlog "Found generic '$generic' with deps: '$depends' and prereq '$prereq'"
        }
    } else {
        set depends $db_depends($target)
		if { [info exists db_prereq($target)] } {
			set prereq $db_prereq($target)
		}
    }

	set result "(reason unknown)"
	if { $hasdepends } {
		vlog "Has dependencies: $depends"
		foreach depend $depends {
			vlog "Considering $depend as dependency for $target"
			if { [catch {build_make_tree $depend $target} result] } {
				set status 0
				vlog "Making $depend failed, so $target won't be made"
				if { $mkv::p::keep_going } {
					vlog "- although continuing with other targets (-k)"
				} else {
					break
				}
			}
		}

		vlog "Has prerequisites: $prereq"

		# For prereq, just make sure that they exist.
		foreach p $prereq {
			vlog "Considering $p as prerequisite for $target"
			if { ![file exists $p] } {
				if { [catch {build_make_tree $p $target} result] } {
					set status 0
					vlog "Making $p failed, so $target won't be made"
					if { $mkv::p::keep_going } {
						vlog "- although continuing with other targets (-k)"
					} else {
						break
					}
				}
			}
		}
	}

 	if { $status == 0 } {
 		error "+++ Make failed for '$depend':\n$result"
 	}

	# If a phony target doesn't have action, it won't be checked for generic action, too.
	if { [info exists db_phony($target)] && ![info exists db_actions($target)] } {
		vlog "Target '$target' is phony and has no action - skipping"
		return
	}

	set stale 0
	
	set need_build 0
	set reason "is wrong"
	if { [info exists db_phony($target)] } {
		set need_build 1
		set reason "is phony"
	} elseif { ![file exists $target] } {
		set need_build 1
		set reason "is missing"
	}

	if { $need_build } {
		vlog "File '$target' $reason - performing action for '$target'"
		if { ![perform_action $target $target $depparent] } {
            error "Action failure for '$target'"
        }
	} elseif { $hasdepends } {
		vlog "Checking if $target is fresh:"
		foreach depend $depends {
            vlog "... against $depend [expr {[info exists db_phony($depend)] ? "(PHONY)":""}]..."
		    set stale [is_target_stale $target $depend]
			if { $stale } {
		        vlog "File '$target' is stale against '$depend', will be made"
		        break
		    }
		}

		if { $stale } {
			vlog "Performing make action for $target"
			if {![perform_action $target $target $depparent]} {
				error "Action failure for $target"
			}
		} else {
			vlog "File $target is fresh, so won't be made"
		}
	} else {
		vlog "File $target exists and has no dependencies, so won't be made"
	}
}

proc make target {

	set mkv::p::action_ringp 0

	set pending_blocks ""

	# Structure:
	# { {commands...} pending_position target }
	# Every pending block represents one target action to be done
	# It's predicted that the pending command is to be executed
	# together with the first with another block.
	# Of course, usually this will be just one command.
	# In order to succeed the block, all commands must succeed.
	# The command must return with a report in orer to know what
	# to do with it next. If it was the last command and it was 
	# successful, then the associated target is moved to q_done.
	# If the command failed, it's done after the failed command,
	# although it's moved to q_done with status = 1.

	build_make_tree $target ""

	while 1 {

		set targets2start [dequeue_action]
		if { $targets2start == "" } {
			break
		}

		foreach t $targets2start {

		}
	}
}

proc apply_action_options action {
	# reset options
	set mkv::p::quiet 0
	set mkv::p::ignore 0
	set mkv::p::escaped 0
	
	while 1 {
		set first [string index $action 0]
		if { $first == "@" } {
		        set mkv::p::quiet 1
		} elseif { $first == "-" } {
		        set mkv::p::ignore 1
		} elseif { $first == "!" } {
		        set mkv::p::escaped 1
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
	variable db_depends

	set depends $db_depends($target)

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

	variable db_depends
	variable db_generic
	variable db_phony
	variable db_actions

	if { ![info exists db_generic] } return

	foreach rule [array names db_generic] {
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

proc enqueue_action {action_target target depparent} {
	variable q_done
	variable q_pending

	# Check if the target is already done
	set p [lsearch -index 0 $q_done $target]

	if { $p != -1 } {
		# It's done, so don't try to do it again.
		$mkv::debug "Not enqueuing $target - already done"
		return
	}

	# Now check if already enqueued
	set p [lsearch -index 0 $q_pending $target]
	if { $p != -1 } {
		# Already enqueued, so don't enqueue it again
		$mkv::debug "Not enqueuing $target - already enqueued"
		return
	}

	# Now check dependent targets if they are declared as failed
	if { [info exists db_depends($target)] } {
		foreach d $db_depends($target) {
			set p [lsearch -index 0 $q_done $target]
			set failed [lindex $q_done $p 1]
			if { $failed } {
				$mkv::debug "Not enqueuing $target - dependency $d failed"
				lappend q_done [list $target 2]
				return
			}
		}
	}

	# Ok, ready to be enqueued
	lappend q_pending [list $target $action_target $depparent]
}

# Two arguments mean:
# target - name of target which identifies actions to be taken
# actual_target - name of target, which is actually being made

# If action is performed normally, both targets are the same
# If action is taken as a link to another target, second argument
# is the target actually built; first is only the indetifier of
# the action, which should be taken.
proc perform_action {actual_target target depparent} {

	variable db_depends
	variable db_generic
	variable db_phony
	variable db_actions

	set sake ""
	if { $target != $actual_target } {
		set sake " for the sake of '$actual_target'"
	}

    vlog "Performing action defined for '$target'$sake"
	set generic ""
	
	if { ![info exists db_actions($target)] } {
        vlog "No specific actions found for '$target' - checking generic actions"
		set generic [find_generic $actual_target]
		if { $generic == "" } {
			puts stderr "+++ No rule to make target '$target'"
            lappend mkv::p::failed $target
            return false
		} else {
            vlog "Found generic '$generic' applicable for target '$target'"
        }
	}

	set actions ""
	set depends ""
	if { $generic == "" } {
		vlog "Performing action (direct):\n>>> $db_actions($target)"
		set depends $db_depends($actual_target)
		set actions [apply_special_variables $db_actions($target) $actual_target]
	} else {
		vlog "Performing action (generic):\n>>> $db_actions($generic)"
		set depends [generate_depends $actual_target $generic $db_depends($generic)]

		# Update dependencies for generated generic target
		set db_depends($actual_target) $depends
		set actions [apply_special_variables $db_actions($generic) $actual_target]
	}

	if { [catch {
		# XXX Action variable substitution replaced only here!
		set actions [subst_action $actions]
	} result] } {
		vlog "Substitution failed: $result"
		error $::errorInfo
	}

	vlog "ACTUAL ACTION: $actions"
	#Set special values

	foreach action $actions {
        set mkv::p::action_performed 1
		# apply standard make options

		set action [apply_action_options [flatten $action]]

		if { $mkv::p::escaped } {
			set command [lindex $action 0]
			set arglist [lrange $action 1 end]

			switch -- $command {
				link {
					set linked_target $arglist
					# XXX handle multiple targets in !link command
					if { ![info exists db_actions($linked_target)] } {
						puts stderr \
						     "+++ Can't resolve $linked_target as a link to action"
						lappend mkv::p::failed $target
						return false
					}

					# Do substitution before altering target
					if { ![perform_action $target $linked_target $depparent] } {
						lappend mkv::p::failed $target
						return false
					}
					continue
				}

				tcl {
					if { !$mkv::p::quiet } {
						puts "CUSTOM COMMAND: $arglist"
					}
					uplevel #0 [list eval $arglist]
					continue
				}
			}

		}

		if { !$mkv::p::quiet } {
			puts $action
		}

		set waserr [catch {exec $mkv::p::shell -c $action 2>@stderr >@stdout} result]; set retcode [pget ::errorCode]
		set failed [expr {$retcode != "NONE"}]
		if { $waserr } {
		        if { $result != "" } {
		                puts stderr $result
		        }
		        if { $failed } {
                   if { !$mkv::p::ignore } {
		                puts stderr "+++ Action for '$actual_target' failed!"
		                lappend mkv::p::failed $target
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

	variable db_depends
	variable db_actions
	variable db_generic
	variable db_phony
	variable db_prereq

	set autoclean_candidates ""
	$debug "TO CLEAN $rule:"
	set deps ""
	set hasrule 0
	if { [info exists db_depends($rule)] } {
		lappend deps {*}$db_depends($rule)
		set hasrule 1
	}
	if { [info exists db_prereq($rule)] } { lappend deps {*}$db_prereq($rule) }
	# WTF? under db_generic($x) there are generic ACTIONS!
	#if { [info exists db_generic($rule)] } { lappend deps {*}$db_generic($rule) }
	set generic [find_generic $rule]
	if { $generic != "" } {
		lappend deps {*}[generate_depends $rule $generic $db_depends($generic)]
		lappend deps {*}[generate_depends $rule $generic $db_prereq($generic)]
	}

	if { $deps == "" } {
		if { [info exists db_depends($rule)] && [info exists db_actions($rule)] } {
			$debug "WILL DELETE: $rule - no dependencies, but has a build rule"
			lappend autoclean_candidates $rule
		} else {
			$debug "WON'T DELETE: $rule - no dependencies (generics: $generic)"
		}
	} else {
		if { ![info exists db_actions($rule)] && $generic == "" } {
			$debug "WON'T DELETE: $rule - no action (phony rule)"
		} else {
			$debug "WILL DELETE: $rule (built from $deps)"
			lappend autoclean_candidates $rule
		}
	}

	incr ::gg_debug_indent
	foreach dep $deps {
		lappend autoclean_candidates {*}[rolling_autoclean $dep $debug]
	}
	incr ::gg_debug_indent -1
	return $autoclean_candidates
}

proc autoclean {rule} {
	set mkv::p::gg_debug_indent 0
	set ac [rolling_autoclean $rule $mkv::debug]
	if { $ac != "" } {
		puts stderr "Autoclean deletes: $ac"
		file delete {*}$ac
	}
}

proc autoclean-test {rule} {
	set mkv::p::gg_debug_indent 0
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
	namespace inscope :: proc $name {} "return {$args}"
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

set public_export [puncomment {

	# Main make facilities
	phony
	rule
	rules
	make

	# Logging
	vlog

	# Utility functions
	pset
	pset+
	pget
	phas
	pdef
	pdefx
	plist
	pexpand
	puncomment
	autoclean
	autoclean-test
	process-options
}]

set public_import ""
foreach n $public_export {
	lappend public_import "mkv::p::$n"
}


namespace export {*}$public_export

}

variable debug_on 0
variable makefile {}

proc MAKE {} {
	return [file normalize $::argv0]
}

}


package provide make 0.5

namespace import {*}$mkv::p::public_import


proc MakeInteractive {argv} {

	set help 0

	set g_optargs {
		-k *keep_going
		-f makefile
		--help *help
		-help *help
		-d *display_debug
		-v *verbose
		-C makefiledir
	}

	set keep_going 0
	set display_debug 0
	set verbose 0
	set makefile {}
	set help 0
	set makefiledir .

	lassign [process-options $argv $g_optargs] cmd_args cmd_variables

	if { $help } {
		puts stderr "Options:"
		foreach {opt arg} $g_optargs {
			puts stderr [format "  %-8s %s" $opt: $arg]
		}
		exit 1
	}

	unset help
	set mkv::p::keep_going $keep_going
	set mkv::makefile $makefile
	set mkv::debug_on $display_debug
	set mkv::p::verbose $verbose
	set mkv::directory $makefiledir

	if { $display_debug } {
		set mkv::debug mkv::p::debug
	}

	foreach {name value} $cmd_variables {
		#puts "Setting $name to $value"
		set $name $value
	}

# --- SET DIRECTORY - before looking for makefile ---
	set wd [pwd]
	set makefiledir [file normalize $makefiledir]
	cd $makefiledir

	if { $mkv::makefile == "" } {
		if { [file exists Makefile.tcl] } {
			set mkv::makefile Makefile.tcl
		} elseif { [file exists makefile.tcl] } {
			set mkv::makefile makefile.tcl
		} else {
			error "Makefile.tcl not found"
		}
	} elseif { ![file exists $mkv::makefile] } {
		error "Makefile '$mkv::makefile' not found"
	}

	$mkv::debug "Sourcing makefile"
	source $mkv::makefile
	$mkv::debug "Executing statements"

	if {$cmd_args == ""} {set cmd_args $mkv::p::first_rule}

	if { [set xc [catch {
		$mkv::debug "BUG $cmd_args $mkv::p::first_rule"
		foreach tar $cmd_args {
			$mkv::debug "Applying make to '$tar'"
			make $tar
		}
	} result]] } {

		puts stderr $result
		puts stderr "+++ Target '$tar' failed"
		return 1
	}

	if { !$mkv::p::action_performed } {
		puts stderr "+++ Nothing to be done for: $cmd_args"
	}

	cd $wd

}


# Rest of the file is interactive.
if { !$tcl_interactive } {

	MakeInteractive $argv

} ;# end of interactive actions
