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

set maxjobs 1

# structure {actual_target target whoneedstarget}
variable q_pending

# List of targets that have been scheduled, but haven't been finished
variable q_running ""

# structure {target status}, where status is
# 0 - succeeded (dependent targets can be started)
# 1 - failed first
# 2 - dropped because dependent target failed
variable q_done {}
variable q_pending {}


variable first_rule {}

variable failed {}
variable skipped {}

variable action_performed 0

set shell {/bin/bash}
if { [info exists ::env(SHELL)] } { set shell $::env(SHELL) }

# May need to be configurable somehow (it's /C on windows cmd)
set shellcmdopt -c

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


# Parallel build support.

# Usage:
# 1. Prepare the initial set of parallel command packs.
#    Every pack may contain multiple commands to be executed sequentially.
#    Subsequent commands are provided in separate lines.
#    Every pack is executed line by line, although multiple packs
#    can be started simulaneously parallelly.
#    Use [ppadd <command>...] where every command can be multi-line command pack.
#    This command only reads every "command" (being a kind-of shell script)
#    and turns it into a list of single commands to execute.
# 2. Pass the prepared command packs to [pprun] command
#    The [pprun] command requires:
#      - global variable for a tracer (of your choice)
#      - the "channels" (list of initial command packs)
#      - callback "vblank" command
#    The "vblank command" receives "references" (names to be used with [upvar])
#    to two database variables (dictionaries):
#    - the current running process database
#    - the finished process database
#    in short: $vblank running finished
# 3. Your vblank callback is free to do the following:
#    - it can add new processes to the running database using [ppupdate] command
#    - it can trace what the current status is

# The [pprepare] function prepares the line-oriented set of commands
# to be run sequentially. This is a required filter before putting it
# to [ppupdate].
proc pprepare {cmd} {
	set res ""

	set channels ""

	vlog ":: pprepare: '$cmd' (original)"
	set cmd [string map {"\\\n" "" "\n\\" ""} $cmd]
	vlog ":: pprepare: '$cmd' (unbackslashed)"
	set cmdlines [split $cmd \n]
	set cmdset ""
	vlog ":: Processing cmdlines: $cmdlines"
	foreach c $cmdlines {
		set c [string trim $c]
		if { $c == "" || [string index $c 0] == "#" } {
			continue
		}

		lappend cmdset $c
	}
	vlog ":: PROCESSED command set: '$cmdset'"
	return $cmdset
}

# The [ppupdate] function updates the given resource database passed
# as the first variable reference and the channels. Channels consist of:
# { channel ... }
#    where 'channel' is:
#    { target cmdset }
#        where 'target' is the displayable target name
#        and 'cmdset' is a line-by-line command set as
#        filtered by [pprepare].
proc ppupdate {r_res channels} {
	upvar $r_res res
	set ids [dict keys $res]

	set howmany [llength $channels]
	set newids ""
	set idroll 0
	while { $howmany } {
		if { $idroll in $ids } {
			incr idroll
			continue
		}
		lappend newids $idroll
		incr idroll
		incr howmany -1
	}

	vlog ":: Channels: '$channels'"

	foreach id $newids channel $channels {
		lassign $channel target cmdset

		vlog ":: Scheduling '$target' as command set: '$cmdset'"

		dict set res $id target $target
		dict set res $id cmdset $cmdset
		dict set res $id pending $cmdset
		dict set res $id fd ""
		dict set res $id running ""
		dict set res $id flags ""
	}

	# This is an optional return value, just FYI.
	# The pprun command doesn't use it.
	return $newids
}

# Prepare the initial channel database and start
# the first portion of commands.
# @param tracername Variable that is used as a synchronizer (of your choice)
# @param channels A list of { {target cmdset}... }, as described at [ppupdate]
# @param vblank A callback function that will get 'res ret' arguments
#               being references to the 'res' (pending) and 'ret' (finished).
#               The vblank function is run every time all channels have been 'reviewed'.
#               The vblank function is allowed to add new execution channels,
#               that's more-less why it receives these two databases.
proc pprun {tracername channels {vblank {}}} {

	set res ""
	set deadcount 0
	set ret ""

	# Prepare initial running process database
	ppupdate res $channels

	while 1 {
		set ids [dict keys $res]
        foreach id $ids {
			set fd [dict get $res $id fd]
			if { $fd == "" } {
				# This means that the block is prepared to run, but
				# the command hasn't been started yet.
				# Start the command that is next in the queue.

				set cmdset [dict get $res $id pending]
				set next [lassign $cmdset cmd]

				set flags ""
				while 1 {
					set flg [string index $cmd 0]
					if { $flg == "@" } {
						lappend flags silent
						set cmd [string range $cmd 1 end]
						continue
					} elseif { $flg == "-" } {
						lappend flags ignore
						set cmd [string range $cmd 1 end]
						continue
					} else {
						break
					}
				}

				dict set res $id running $cmd
				dict set res $id pending $next
				dict set res $id flags $flags
				if { $cmd == "" } {
					# Nothing more to run. Consider commandset succeeded.
					continue
				}

				# You may need to resolve the command as Tcl command
				set cmdcmd [lindex $cmd 0]
				if { $cmdcmd == "!tcl" } {
					set arglist [lrange $cmd 1 end]
					# DO NOT schedule this command - execute it directly
					if { !$mkv::p::quiet } {
						puts "%$arglist"
					}
					uplevel #0 [list eval $arglist]

					# Leave the state as is. Next iteration will pick up
					# the next command and do whatever's necessary.
					# Most likely there's just one !tcl command, so it
					# will be empty and the channel will be closed.
					continue
				}

				if { !$mkv::p::quiet && "silent" ni $flags } {
					puts stderr "\[$id\]> $cmd"
				}

				set external "$mkv::p::shell $mkv::p::shellcmdopt {$cmd} 2>@stderr"
				puts "RUNNING: {$external}"

				set fd [open "|$external"]
				dict set res $id fd $fd

				fconfigure $fd -blocking 0 -buffering line
    			fileevent $fd readable "set $tracername $fd"
			}

			# Ok, now it is running at least the first command
			# (that is, $fd is nonempty)

			if {[eof $fd]} {

				# Turn of O_NDELAY or otherwise the error won't be seen!
				fconfigure $fd -blocking 1

				set infotext "\[$id\] "
				# The currently running command has finished.
				# If not to be ignored, check the exit code.
				set err [catch {close $fd} cres copts]
				set code [dict get $copts -code]
				if { $code != 0 } {
					append infotext " ***Error "
					set ec [pget $copts.-errorcode]
					if { [lindex $ec 0] == "CHILDSTATUS" } {
						set ec [lindex $ec 2]
						append infotext " $ec"
					} else {
						set ec -1
					}
					if { "ignore" in $flags } {
						append infotext "(ignored) "
					} else {
						set deadkey $id.$deadcount
						incr deadcount

						# The command failed, so interrupt the sequence right now.
						dict set ret $deadkey target [dict get $res $id target]
						dict set ret $deadkey cmdset [dict get $res $id cmdset]
						dict set ret $deadkey code $code
						dict set ret $deadkey error $ec
						dict set ret $deadkey failed [dict get $res $id running]
						dict set ret $deadkey result $cres
						dict set ret $deadkey options $copts

						# and remove the channel from the res list
						set res [dict remove $res $id]
						puts stderr "$infotext: $cmd"
						continue
					}

					# Continue, if the errors for $cmd should be ignored.
				}

				# Either succeeded, or maybe failed, but ignored.
				# Whatever way, finished.

				# So, pick up the next command from the list
				set next [dict get $res $id pending]
				if { $next == "" } {
					# Hooray! We've done all commands and all were successful.
					set deadkey $id.$deadcount
					incr deadcount

					dict set ret $deadkey target [dict get $res $id target]
					dict set ret $deadkey cmdset [dict get $res $id cmdset]
					dict set ret $deadkey code 0
					dict set ret $deadkey error ""
					dict set ret $deadkey failed "" 
					dict set ret $deadkey result $cres 
					dict set ret $deadkey options $copts
					set res [dict remove $res $id]
					continue
				}

				# Ok, so if we have "next", clear the fd so that the next
				# roll will know that it should pick up and start the next command.
				dict set res $id fd ""

			} else {
				# Roll until EOF or EAGAIN.
				# When EAGAIN, it will be retried at the next roll.
				# When EOF, it will be removed from the list at the next roll.
				while { [gets $fd linein] != -1 } {
					puts "\[$id\] $linein"
				}
			}
        }

		if { $vblank != "" } {

			# The vblank callback could have modified the 'res' variable, and
			# add new command sequences, even though the previous loop has made
			# them all wiped out.
			apply $vblank res ret
		}

		if { $res == "" } {
			# All processes finished.
			break
        }

		# Ok, now clear the variable
		set $tracername ""

		# If vwait causes error, it may happen that the process
		# has finished before it could be added to the event list
		# (just the script had no opportunity to see it). If this
		# happens, just go on, and in the next roll you'll find it out anyway.
		catch {vwait $tracername}

		#puts "UNBLOCKED BY: [set $tracername]"
    }
    return $ret
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

proc getdeps { target {depnames {}} } {
	set depends ""
	if { $depnames == "" } {
		variable db_depends
		set depnames $db_depends($target)
	}
	foreach d $depnames {
		if { [string index $d 0] == "<" } {
			set rf [string range $d 1 end]
			set rule [load-rule $target $rf]
			lappend depends {*}$rule $rf

			# Here you can handle other cases of special characters
			# Except @ of course.
		} else {
			lappend depends $d
		}
	}
	return $depends
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

proc load-rule {target rulefile} {
	variable db_actions

	vlog " --- Expecting that ingredients for $target are found in $rulefile"
	if { [info exists db_actions($rulefile)] } {
		# Make, no matter whether exists or not.
		# May need to be refreshed.
		vlog " -- Executing rules to make '$rulefile'"
		make $rulefile
	} else {
		vlog " ??? NO RULES to make '$rulefile' - expecting that it just exists..."
	}

	if { ![file exists $rulefile] } {
		error "*** No rule to make dependency file '$rulefile' for target '$target'"
	}

	set fd [open $rulefile r]
	set rule [read $fd]
	close $fd

	$mkv::debug "RULE FROM FILE '$rulefile': ingredients: $rule"

	return $rule
}

proc rule args {
	variable db_depends
	variable db_generic
	variable db_phony
	variable db_actions
	variable db_prereq

	#$mkv::debug " #1 RULE ARGS: $args"
	set args [expandif @ $args]
	#$mkv::debug " #2 RULE ARGS: $args"
	set size [llength $args]
	if { $size < 2 } {
		return -code error "+++ rule: at least two arguments required (name,action)"
	}
	# assert { $size >= 2 }

	set target [lindex $args 0]
	if { [string index [lindex $target 0] end] == ":" } {

		# In this case this should contain exactly two arguments.
		# If any more, issue an error
		if { $size != 2 } {
			error "Syntax target:depends requires rule {deps} {action}. Incorrect number $size of arguments."
		}

		set target [string map {"\\\n" ""} $target]

		# Target contains now target with dep list, actionspec is the second
		set actionspec [lindex $args end]

		# This will be now TARGET: DEP DEP DEP
		set args $target
		# ... and size, as it was also already set

		# Cut off : (target was already set, the args[0] will be ignored)
		set target [string range [lindex $target 0] 0 end-1]
		$mkv::debug "Using makefile syntax: TARGET: $target DEPS: [lrange $args 1 end]"

		# Put it back in place
		lappend args $actionspec
		set size [llength $args]
	}
	#$mkv::debug " #3 RULE ARGS: $args"
	#$mkv::debug [join $args ;]

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
	if { [check_generic_upd_first $target] } {
		$mkv::debug "RULE '$target' is generic: {$action}"
		set db_generic($target) $action
	}
	
	$mkv::debug "RULE '$target' DEPS: $depends PREREQ: $prereq"
	$mkv::debug "... ACTION: {$action}"
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
	# remove all "* ? [ ]", but leave untouched "\* \? \[ \]"
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

proc gendep {depfile args} {
	set origdeps [exec {*}$args]

	# First, remove these stupid line breaks
	set deps [string map {"\\\n" " "} $origdeps]

	# Second, delete the first word which is the name of the target
	# (we'll have the name set explicitly)
	set deps [lrange $deps 1 end]

	# And now write them into the depfile
	set fd [open $depfile w]
	puts $fd $deps
	close $fd
}

proc dep-rule {depfile sourcefile action} {
	rule $depfile $sourcefile "!tcl gendep $depfile [string trim $action]"
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

	$mkv::debug "Substituting: $lines_target"

	set o [catch {
	set target [uplevel #0 subst [list $lines_target]]
	}]
	if { $o } {
		vlog ERROR:$::errorInfo
	}
	$mkv::debug "Substituted: ---> $target"

	return $target
}

proc is_target_stale {target depend} {
	variable db_phony
	variable db_depends

	# Check if $depend is phony.
	# If phony, then just forward the request to its depends.
	# XXX THIS FEATURE IS SLIGHTLY CONTROVERSIAL.
	# Probably another type of target "forward" should exist, parallel to "phony".
	if { [info exists db_phony($depend)] } {
		set out false
		if { [info exists db_depends($depend)] && [expr {$db_depends($depend) != ""}] } {
			vlog "... ... and has deps ..."
			foreach d [getdeps $target] {
				vlog "... ... forwarding: against $d ... [expr {[info exists db_phony($d)] ? "(also phony)" : ""}]"
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

variable depth
set depth 0
variable verbose
set verbose 0

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

proc build_make_tree {target whoneedstarget} {

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
            set depnames [generate_depends $target $generic $db_depends($generic)]
            if { $depnames != "" } {
                set hasdepends 1
            }
			if { [info exists db_prereq($generic)] } {
				set prereq [generate_depends $target $generic $db_prereq($generic)]
			}
			vlog "Found generic '$generic' with deps: '$depnames' and prereq '$prereq'"
        }
    } else {
        set depnames $db_depends($target)
		if { [info exists db_prereq($target)] } {
			set prereq $db_prereq($target)
		}
    }

	set result "(reason unknown)"
	if { $hasdepends } {
		vlog "Has dependencies: $depnames"

		# Resolve possible file-contained dependencies

		set depends [getdeps $target $depnames]
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
		vlog "File '$target' $reason - resolving action for '$target'"
		if { ![resolve_action $target $target $whoneedstarget] } {
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
			vlog "Resolving make action for $target (stale against $depend)"
			if {![resolve_action $target $target $whoneedstarget]} {
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

	vlog "--- Target queue prepared - starting action"
	variable q_pending
	$mkv::debug "::: Action queue: [llength $q_pending]"

    foreach e $q_pending {
    	lassign $e target ac need
    	$mkv::debug "$target:  (need by $need): [string map {"\n" "\\n"} $ac]"
    }

	variable q_running
	variable failed
	variable q_done

	if { $mkv::p::maxjobs == 1 } {
		# Do normal processing - dequeue always one action

		while 1 {
			set target2start [dequeue_action 1]
			vlog "--- DEQUEUED: $target2start"
			if { $target2start == "" } {
				break
			}
			# There's only one, so extract always the first element
			lassign [lindex $target2start 0] target action whoneedstarget
			vlog "--- TARGET: $target ACTION: $action PARENT: $whoneedstarget"

			# Add this target to the running targets
			lappend q_running $target
			vlog "--- CURRENTLY RUNNING: $q_running"

			set channel [list $target [pprepare $action]]
			lappend channels $channel
			pprun ::mkv::p::tracer $channels {{r_res r_ret} {
				upvar ::mkv::p::q_running q_running
				upvar ::mkv::p::q_done q_done
				upvar ::mkv::p::failed failed
				vlog "+++ VBLANK RUNNING: $q_running"
				upvar $r_res res
				upvar $r_ret ret
				puts stderr "VBLANK: res=$res ret=$ret"

				# This is one-shot. Do nothing if no action has been finished.
				if { $ret == "" } {
					return
				}

				# Check which actions have been finished. Remove them from q_running.
				# We have here just one action
				lassign $ret deadkey retdb
				set target [dict get $retdb target]
				vlog "Found finished target: $target"

				# Remove the target from running
				set q_running [plremove $q_running $target]

				# Check the status
				set failed [dict get $retdb failed]
				if { $failed != "" } {
					lappend failed $target
				}

				# Add to done targets (regardless of the status)
				lappend q_done $target

				# This should remove from ret - but in ret there should be only one
				set ret ""

				# Get the next action that can be done
				set target2start [mkv::p::dequeue_action 1]
				lassign [lindex $target2start 0] target action whoneedstarget

				if { $target != "" } {
					set channel [list $target [mkv::p::pprepare $action]]
					# Schedule next action
					mkv::p::ppupdate res [list $channel]
				}
				# Don't schedule anything if empty

				vlog "+++ NOW RUNNING: $q_running"

			}}

			#error "NOT CONTINUING INFINITE LOOP"
		}

		return
	}

	# Go parallel
	error "Parallel not yet supported"
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
		#vlog " ... against fresher depend $d"
		if { [is_target_stale $target $d] } {
			lappend out $d
		}
	}

	return $out
}

proc apply_special_variables {action target} {
	variable db_depends

	set depends [getdeps $target]

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

proc enqueue_action {target action whoneedstarget} {
	variable q_done
	variable q_pending

	# Check if the target is already done
	set p [lsearch -index 0 $q_done $target]

	if { $p != -1 } {
		# It's done, so don't try to do it again.
		$mkv::debug "Not enqueuing $target - already done"
		return pass
	}

	# Now check if already enqueued
	set p [lsearch -index 0 $q_pending $target]
	if { $p != -1 } {
		# Already enqueued, so don't enqueue it again
		$mkv::debug "Not enqueuing $target - already enqueued"
		return pass
	}

	# Ok, ready to be enqueued
	lappend q_pending [list $target $action $whoneedstarget]
	return pass
}

proc dequeue_action {nrequired} {
	set out ""
	set nqueued 0
	variable q_done
	variable q_pending
	variable q_running
	variable failed
	variable keep_going

	# Pick up at most $nrequired actions from the pending queue.
	# Detect if $whoneedstarget is already in 'failed'. If so, then
	# depending on keep_going option, just skip targets that
	# depend on already failed targets, or return empty list
	# immediately.

	# Review the list of q_pending.
	# Split the iterated targets into two lists:
	# - ready: those that can be sent to building now
	# - stalled: those that need dependent targets to be finished
	# Roll until the end or until the number of 'ready' targets
	# is equal to the $nrequired.
	# At the end, do: 
	#
	# set q_pending [concat $stalled [lrange $q_pending $pos end]]
	# (in case when $pos reached $size, the second list will be empty)
	#
	# and return $ready
	#
	# If you find any dependency failed, mark the dependent target failed, too.
	# If this was found in case when !$keep_going,
	#     CLEAR THE q_pending and return nothing.
	#
	# It's expected that the scheduling machine roll until the q_pending
	# list becomes empty - even if this function return an empty string
	# (it may happen that all targets in q_pending depend on some target
	# that is still building).

	set faildep ""
	set size [llength $q_pending]
	for {set pos 0} {$pos < $size} {incr pos} {
		set pend [lindex $q_pending $pos]
		lassign $pend target action whoneedstarget

		vlog "+++Considering target: $target"
		vlog "+++Running now: $q_running"
		vlog "+++Done targets: $q_done"

		if { $target in $q_done } {
			if { $target in $failed } {
				vlog " --- Not making '$target' - already failed"
				continue
			}

			vlog " --- Not making '$target' - already done"
			continue
		}

		if { $target in $q_running } {
			vlog " --- already scheduled '$target', try another one"
			continue
		}

		# Now check dependent targets if they are declared as failed
		if { [info exists db_depends($target)] } {
			foreach d [getdeps $target] {
				if { $d in $failed } {
					vlog " --- Not making $target - dependency $d failed"
					lappend q_done [list $target 2]
					return ""
				}
			}
		}

		lappend out [list $target $action $whoneedstarget]
		incr nqueued
		if { $nqueued == $nrequired } {
			break
		}
	}

	set q_pending [lrange $q_pending $pos end]

	# May happen that nothing has been extracted or less have been
	# extracted than it was requested.

	$mkv::debug "DEQUEUED TO RUN: $out"
	return $out
}

# Arguments:
# actual_target - name of target, which is actually being made
# target - name of target which identifies actions to be taken
# whoneedstarget - the target this one is needed for

# If action is performed normally, both targets are the same
# If action is taken as a link to another target, second argument
# is the target actually built; first is only the indetifier of
# the action, which should be taken.
proc resolve_action {actual_target target whoneedstarget} {

	variable db_depends
	variable db_generic
	variable db_phony
	variable db_actions

	set sake ""
	if { $target != $actual_target } {
		set sake " for the sake of '$actual_target'"
	}

    vlog "Resolving action defined for '$target'$sake"
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
		vlog "Resolving action (direct):\n>>> $db_actions($target)"
		set depends $db_depends($actual_target)
		set actions [apply_special_variables $db_actions($target) $actual_target]
	} else {
		vlog "Resolving action (generic):\n>>> $db_actions($generic)"
		set depends [generate_depends $actual_target $generic $db_depends($generic)]

		# Update dependencies for generated generic target
		set db_depends($actual_target) $depends
		set actions [apply_special_variables $db_actions($generic) $actual_target]
	}

	vlog "Actions resolved:\n>>> $actions"

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
					if { ![resolve_action $target $linked_target $whoneedstarget] } {
						lappend mkv::p::failed $target
						return false
					}
					continue
				}

				tcl {
					if { !$mkv::p::quiet } {
						puts "%$arglist"
					}
					uplevel #0 [list eval $arglist]
					continue
				}
			}

		}

		# The remaining 'action' is the extracted commandset.
		# Now enqueue the action.
		$mkv::debug "ENQUEUING $actual_target with ACTION: $action"
	
		if { [catch {enqueue_action $actual_target $action $whoneedstarget} error] } {
			puts stderr "ERROR ENQUEUING: $error"
		}

# ---	if { !$mkv::p::quiet } {
# ---		puts $action
# ---	}
# ---
# ---	set waserr [catch {exec $mkv::p::shell $mkv::p::shellcmdopt $action 2>@stderr >@stdout} result]; set retcode [pget ::errorCode]
# ---	set failed [expr {$retcode != "NONE"}]
# ---	if { $waserr } {
# ---		if { $result != "" } {
# ---			puts stderr $result
# ---		}
# ---		if { $failed } {
# ---			if { !$mkv::p::ignore } {
# ---				puts stderr "+++ Action for '$actual_target' failed!"
# ---				lappend mkv::p::failed $target
# ---				return false
# ---			} else {
# ---				puts stderr "+++ Action for '$actual_target' failed (but ignored)."
# ---			}
# ---		}
# ---	} else {
# ---		if { $result != "" } {
# ---			puts $result
# ---		}
# ---	}
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

	# Extract and filter out dependency files
	set od ""
	foreach dep $deps {
		if { [string index $dep 0] == "<" } {
			set rf [string range $dep 1 end]
			set dp [load-rule $rule $rf]
			lappend od {*}$dp $rf
		} else {
			lappend od $dep
		}
	}
	set deps $od

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
        append var " $a"
    }
}

proc plremove {list item} {
	return [lsearch -not -exact -all -inline $list $item]
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
	if { [llength $args] == 1 } {
		set args [lindex $args 0]
	}
	namespace inscope :: proc $name {} "return \[uplevel concat $args\]"
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
	gendep
	dep-rule
	make

	# Logging
	vlog

	# Utility functions
	plremove
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
	set path [file normalize $::argv0]
	set options ""
	variable debug_on
	if { $debug_on } {
		append options " -d"
	}


	if { $::mkv::p::verbose } {
		append options " -v"
	}

	return $path$options
}

}


package provide make 0.5

namespace import {*}$mkv::p::public_import

# Rest of the file is interactive.
if { !$tcl_interactive } {



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

	$mkv::debug "SUMMARY DATABASE (direct targets):"
	foreach n [array names mkv::p::db_depends] {
		$mkv::debug "$n: $mkv::p::db_depends($n) {[pget mkv::p::db_actions($n)]}"
	}

	$mkv::debug "Executing statements"

	if {$cmd_args == ""} {set cmd_args $mkv::p::first_rule}

	if { [set xc [catch {
		$mkv::debug "WILL MAKE: $cmd_args STARTING FROM $mkv::p::first_rule"
		foreach tar $cmd_args {
			$mkv::debug "Applying make to '$tar'"
			make $tar
		}
	} result]] } {

		puts stderr $result
		puts stderr "+++ Target '$tar' failed: $result"
		return 1
	}

	if { !$mkv::p::action_performed } {
		puts stderr "+++ Nothing to be done for: $cmd_args"
	}

	cd $wd



} ;# end of interactive actions
