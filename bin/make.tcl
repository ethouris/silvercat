#!/bin/bash
# but tcl \
exec tclsh "$0" "$@"

package require Tcl 8.5

# Debug
#package require Itcl



namespace eval mkv {

set debug mkv::p::pass


namespace eval p {

		# Prologue
		set me [info script]
		set here [file dirname $me]

		# Ingredients

		source $here/mkv.p.utilities.tcl
		source $here/mkv.p.builtin.tcl

variable db_depends
variable db_actions
variable db_generic
variable db_phony
variable db_need
variable db_stale
variable db_flags

set maxjobs 1

# structure {actual_target target whoneedstarget}
variable q_pending

# List of targets that have been scheduled, but haven't been finished
variable q_running ""

# Target on which the failure was triggered
variable failtarget ""

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


variable keep_going 0


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
#    Use [ppupdate <command>...] where every command can be multi-line command pack.
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
proc pprepare {cmdlines} {
	set res ""

	set channels ""

    #vlog ":: pprepare: '$cmd' (original)"
    #set cmd [string map {"\\\n" "" "\n\\" ""} $cmd]
    #vlog ":: pprepare: '$cmd' (unbackslashed)"
    #set cmdlines [split $cmd \n]
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
	set njobs [llength $channels]

	$mkv::debug "INITIAL CHANNELS: $channels"

	# Prepare initial running process database
	ppupdate res $channels

	while 1 {
		set ids [dict keys $res]
		foreach id $ids {
			$mkv::debug "CHECKING JOB $id: [dict get $res $id]"
			set fd [dict get $res $id fd]
			if { $fd == "" } {
				# This means that the block is prepared to run, but
				# the command hasn't been started yet.
				# Start the command that is next in the queue.

				set cmdset [dict get $res $id pending]
				set next [lassign $cmdset cmd]
				dict set res $id next $next

				set flags ""
				set cmdspec ""

				while 1 {
					set flg [string index $cmd 0]
					switch -- $flg {
						@ {
							lappend flags silent
						}

						- {
							lappend flags ignore
						}

						! {
							lappend flags cmd
							set cmdspec [string range [lindex $cmd 0] 1 end]
							set cmd [lrange $cmd 1 end]
							break ;# no more possible flags in this case
						}

						% {
							lappend flags cmd
							set cmdspec tcl
						}

						default {
							break
						}
					}

					# Eat up flags until the picked up character isn't a flag
					set cmd [string range $cmd 1 end]
				}

				dict set res $id running $cmd
				dict set res $id pending $next
				dict set res $id flags $flags
				if { $cmd == "" } {
					# Nothing more to run. Consider commandset succeeded.
					continue
				}
				if { $cmdspec != "" } {
					# This is left for future use, but currently the only
					# special command is "tcl".
					switch -- $cmdspec {
						tcl {
							if { "quiet" ni $flags } {
								puts "%$cmd"
							}

							$mkv::debug "TCL COMMAND - executing directly '$cmd' (next: $next)"
							# DO NOT schedule this command - execute it directly
							uplevel #0 [list eval $cmd]

							# Leave the state as is. Next iteration will pick up
							# the next command and do whatever's necessary.
							# Most likely there's just one !tcl command, so it
							# will be empty and the channel will be closed.

							# Make fd nonempty to prevent rescheduling
							dict set res $id fd "-"

							# Remove the command from the queue
							#set res [dict remove $res $id]
							continue
						}

						default {
							error "Unrecognized special command: !$cmdspec for target [dict:at $res $id target]"
						}
					}
				}

				if { $fd == "-" } {
					error "WTF fd - for cmd=$cmd"
				}


				set pfx ""
				if { $mkv::p::maxjobs > 1 } {
					set pfx "\[[expr {$id+1}]/$njobs\]> "
				}

				if { "silent" ni $flags } {
					$mkv::debug "$pfx<[pwd]>"
					puts stderr "$pfx$cmd"
				}

				set external "$mkv::p::shell $mkv::p::shellcmdopt {$cmd} 2>@stderr"
				$mkv::debug "RUNNING: {$external} id=$id fd=$fd; NUMBER OF JOBS: $njobs"

				set fd [open "|$external"]
				dict set res $id fd $fd

				fconfigure $fd -blocking 0 -buffering line
				fileevent $fd readable "set $tracername $fd"
			}

			# Ok, now it is running at least the first command
			# (that is, $fd is nonempty)
			# Or it was run as a Tcl command that was executed in place

			if { $fd == "-" } {
				set iseof 0
				set isend 1
				set cres ""
				set copts ""
				set code 0
			} else {
				set iseof [eof $fd]
				set isend $iseof
			}

			if { $isend } {

				# Do this action for external processes only.
				if { $iseof } {

					# FIXED BUG: cmd was set only when starting a command,
					# so here it could simply carry over a singular state.
					set cmd [dict get $res $id running]
					$mkv::debug "FOUND EOF id=$id fd=$fd cmd=$cmd"

					# Turn off O_NDELAY or otherwise the error won't be seen!
					fconfigure $fd -blocking 1

					set infotext "\[[expr {$id+1}]\] "
					# The currently running command has finished.
					# If not to be ignored, check the exit code.
					set err [catch {close $fd} cres copts]
					set code [dict get $copts -code]
					if { $code != 0 } {
						append infotext " +++Error"
						set ec [pget copts.-errorcode]
						if { [lindex $ec 0] == "CHILDSTATUS" } {
							set ec [lindex $ec 2]
							append infotext " $ec"
						} else {
							set ec -1
						}
						if { "ignore" in $flags } {
							append infotext " (ignored)"
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
							if { $njobs > 1 } {
								puts stderr "$infotext: $cmd"
							}
							continue
						}

						# Continue, if the errors for $cmd should be ignored.
					}
				}

				# Either succeeded, or maybe failed, but ignored.
				# Whatever way, finished.

				# So, pick up the next command from the list
				set next [dict get $res $id pending]
				if { $next == "" } {
					# Hooray! We've done all commands and all were successful.
					set deadkey $id.$deadcount
					incr deadcount
					$mkv::debug "FINISHED CMD LIST for '[dict get $res $id target]'"

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
				set pfx ""
				if { $mkv::p::maxjobs > 1 } {
					set pfx "\[$id/$njobs\]  "
				}

				# Roll until EOF or EAGAIN.
				# When EAGAIN, it will be retried at the next roll.
				# When EOF, it will be removed from the list at the next roll.
				while { [gets $fd linein] != -1 } {
					puts "$pfx$linein"
				}
			}
		}

		if { $vblank != "" } {
			# The vblank callback could have modified the 'res' variable, and
			# add new command sequences, even though the previous loop has made
			# them all wiped out.
			apply $vblank res ret

			# VBLANK might have changed the number of jobs, it's actually its job
			set ljobs [expr [llength $res]/2]
			if { $ljobs > $njobs } {
				$mkv::debug "INCREASED JOB SLOT USAGE TO: $ljobs"
				set njobs $ljobs
			}
		}

		if { $res == "" } {
			# All processes finished.
			break
		}

		# Ok, now clear the variable
		set $tracername ""

		# Now check for every target if they all have $fd that are not
		# finished. Targets that have no fd in running state should be ignored
		set nrun 0
		foreach {id data} $res {
			set fd [pget data.fd]
			if { $fd == "" || $fd == "-" } {
				continue
			}
			set isfinished 0
			if { [catch {eof $fd} isfinished] || $isfinished} {
				continue
			}
			incr nrun
		}

		if { $nrun } {

			$mkv::debug "STILL RUNNING: $mkv::p::q_running - waiting for finish any of $nrun running processes"

			# If vwait causes error, it may happen that the process
			# has finished before it could be added to the event list
			# (just the script had no opportunity to see it). If this
			# happens, just go on, and in the next roll you'll find it out anyway.
			catch {vwait $tracername}
		}

		#puts "UNBLOCKED BY: [set $tracername]"
	}
	return $ret
}


proc InterpretDepends { rule deps {r_depfiles ""} } {
	if { $r_depfiles != "" } {
		upvar $r_depfiles depfiles
	} else {
		set depfiles ""
	}

	$mkv::debug "Interpreting deps for '$rule': $deps"

	set od ""
	foreach dep $deps {
		if { [string index $dep 0] == "<" } {
			set rf [string range $dep 1 end]
			set dp [load-rule $rule $rf $od]
			lappend depfiles $rf

			$mkv::debug " --- rules extracted from $rf: $dp"

			foreach d $dp {
				if { $d ni $od } {
					lappend od $d
				}
			}

			# Add the dependency file itself to the dependencies,
			# but ONLY IF THE RULE FOR BUILDING IT EXISTS, OR
			# if this is so far the only rule. In other words,
			# don't make the depfile itself a dependency, if the
			# depfile doesn't exist and it's expected to be generated
			# together with *.o file (in which case we must have at
					# least the source file itself already in $od).
			variable db_actions
			if { [info exists db_actions($rf)] || $od == "" } {
				lappend od $rf
			} else {
				$mkv::debug "NOT adding depfile '$rf' to deps of '$rule': action:'[pget db_actions($rf)]' otherdeps:'$od'"
			}
		} else {
			lappend od $dep
		}
	}
	$mkv::debug "Resulting deps for '$rule': $od"
	return $od
}

proc getdeps { target {depnames {}} } {
	if { $depnames == "" } {
		variable db_depends
		set depnames $db_depends($target)
	}
	return [InterpretDepends $target $depnames]
}

proc getprereq { target } {
	variable db_prereq
	if { ![info exists db_prereq($target)] } {
		return ""
	}

	set deps $db_prereq($target)
	foreach d $deps {
		lappend deps {*}[getprereq $d]
	}

	return $deps
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

proc load-rule {target rulefile others} {
	variable db_actions

	vlog " --- Expecting that ingredients for $target are found in $rulefile"
	if { [info exists db_actions($rulefile)] } {
		# Make, no matter whether exists or not.
		# May need to be refreshed.
		vlog " -- Executing rules to make '$rulefile'"
		make $rulefile
	}

	if { ![file exists $rulefile] } {
		if { $others == "" } {
		        puts stderr "*** NOTE: if depfile is the only depspec, then it must exist or be make-able"
		        error "*** Depfile '$rulefile' for target '$target' not found and there's no rule to make it"
		}

		# We have other rules, so just pretend as if there are no more ingredients.
		return ""
	}

	set fd [open $rulefile r]
	set rule [TranslateDeps [read $fd]]
	close $fd

	$mkv::debug "RULE FROM FILE '$rulefile': ingredients: $rule"

	return $rule
}

proc reload_makefile {} {
	set q_done ""
	foreach ar {
		db_depends
		db_generic
		db_phony
		db_actions
		db_prereq
		db_need
		db_stale
	} {
		variable $ar
		array unset $ar
	}

	uplevel #0 source $mkv::makefile
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


proc gendep {srcdir depfile args} {
	set wd [pwd]
	cd $srcdir
	set indeps [exec {*}$args]
	set deps [TranslateDeps $indeps]

	set deps [pmap [list v "return \[prelocate \"\$v\" \"$wd\"\]"] $deps]

	cd $wd
	# And now write them into the depfile
	pupdate $depfile $deps
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

proc setflags {target args} {
	variable db_flags

	if { $args == "-" } {
		unset db_flags($target)
		return
	}

	set neg 0

	foreach f $args {
		if { $f == "-" } {
			set neg 1
			continue
		}
		if { $neg } {
			set db_flags($target) [lsearch -all -inline -not -exact [pget db_flags($target)] $f]
		} else {
			set db_flags($target) [lsort -unique [concat [pget db_flags($target)] $f]]
		}
	}
}

proc getflags {target} {
	variable db_flags
	return [pget db_flags($target)]
}

proc rules {rulelist action} {
	set rulelist [puncomment $rulelist]
	set firstrule [lindex $rulelist 0]
	set rules [lrange $rulelist 1 end]

	eval "[pflat rule $firstrule] {$action}"
	foreach rule $rules {
		set link [list !link [lindex $firstrule 0]]
		eval "[pflat rule $rule] {$link}"
	}
}

proc subst_action action {
	set action [string map {"\\\n" "" "\n\\" ""} $action]
	set lines [split $action \n]
	set lines_target {}
	foreach action $lines {
		set action [string trim $action]
		if { $action == "" } continue
	    #$mkv::debug " ... action line: $action"
	    #set al [lrange $action 0 end]
	    #$mkv::debug " ... as list: $al"
	#
	    #set target {}
	    #foreach element $action {
	    #        $mkv::debug " ... ELEMENT: $element"
	    #        append target "$element "
	    #}
	    #lappend lines_target $target
		lappend lines_target $action
	}

	$mkv::debug "Substituting: $lines_target"

	set target [uplevel #0 subst -nobackslashes [list $lines_target]]
	$mkv::debug "Substituted: ---> $target"

	return $target
}

proc is_target_stale {target depend} {
	variable db_phony
	variable db_depends
	variable db_stale

	$mkv::debug "is_target_stale: Checking if '$target' is stale against '$depend'"
	if { [info exists db_stale($depend)] } {
		vlog "Target '$depend' is already stale, therefore so is '$target'"
		return true
	}

	# Check if $depend is phony.
	# If phony, then just forward the request to its depends.
	# XXX THIS FEATURE IS SLIGHTLY CONTROVERSIAL.
	# Probably another type of target "forward" should exist, parallel to "phony".
	if { [info exists db_phony($depend)] } {
		set out false
		if { [info exists db_depends($depend)] && [expr {$db_depends($depend) != ""}] } {
			set target_deps [getdeps $depend]
			vlog "... ... and '$depend' has deps: $target_deps"
			foreach d $target_deps {
				if { $d == $target } {
					puts stderr "+++ ERROR: recursive dep $target -> $d - DROPPING."
					continue
				}
				vlog "... ... forwarding to dep of $target: against $d ... [expr {[info exists db_phony($d)] ? "(also phony)" : ""}]"
				mkv::p::debug_indent+
				set stale [is_target_stale $target $d]
				mkv::p::debug_indent-
				set out [expr {$out || $stale} ]
				vlog "... stale: $out"
			}
		} else {
			vlog "... ... and '$depend' has no deps ..."
			return false ;# phony that has no deps means always fresh
		}
		return $out
	}

	if { ![file exists $target] } {
		vlog "... ... '$target' missing - declare it stale."
		return true
	}

	if { ![file exists $depend] } {
		vlog "... ... '$target' dep '$depend' missing - declare target stale."
		return true
	}

	if { [catch {
			set mtime_depend [file mtime $depend]
			set mtime_target [file mtime $target]
			set stale [expr {$mtime_depend > $mtime_target ? yes : no}] ;# this yes/no is in order to print in log
		}]
	} {
		puts stderr "IPE: 'file mtime' error on '$depend' - '$target' declared stale"
		return true
	}

	$mkv::debug "is_target_stale: TARGET: $mtime_target DEPEND: $mtime_depend: STALE: $stale"

	return $stale
}

proc depends {args} {
	variable db_depends
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

proc build_make_tree {parentstack target whoneedstarget} {

	variable db_depends
	variable db_generic
	variable db_phony
	variable db_actions
	variable db_prereq
	variable db_need
	variable db_stale
	variable q_done
	variable depth

	# This needs to trace whatever has interrupted this
	# process directly

	set failgoal $target
	set catch [catch {

		set cyclepos [lsearch -exact $parentstack $target]
		if { $cyclepos != -1 } {
			# This is error - dependency cycle.
			# Build cycle information, display, throw error.

			set cycleinfo [lrange $parentstack $cyclepos end]
			puts stderr "ERROR: DEPENDENCY CYCLE: [join $cycleinfo " --> "] --> $target"
			error "Dependency cycle found at target '$target'"
		}

		$mkv::debug "--- building tree for $target --- (parents: $parentstack)"

		# Deny making targets, which already failed
		if { [lsearch $mkv::p::failed $target] != -1 } {
			vlog "Target $target denied, because already failed"
			error "Derived failure from $target"
		}

		set status 1
		set hasdepends [info exists db_depends($target)] 
		set prereq ""

		if { !$hasdepends } {
			vlog "No direct depends for '$target', checking for generic depends"
			set generic [find_generic $target]
			if { $generic != "" } {
				incr depth
				set depnames [generate_depends $target $generic $db_depends($generic)]
				incr depth -1
				if { $depnames != "" } {
					set hasdepends 1
				}

				# XXX Generic phony prerequisites... should be supported?
				if { [info exists db_prereq($generic)] } {
					incr depth
					set prereq [generate_depends $target $generic $db_prereq($generic)]
					incr depth -1
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
			vlog "($target) Has dependencies: $depnames"

			# Resolve possible file-contained dependencies

			set depends [getdeps $target $depnames]
			foreach depend $depends {
				$mkv::debug "Considering $depend as dependency for $target (parents: $parentstack)"
				debug_indent+
				set res [catch {build_make_tree [concat $parentstack $target] $depend $target} result]
				debug_indent-
				$mkv::debug "... <--- back at '$target'"
				if { $res } {
					$mkv::debug "ERROR: CODE=$::errorCode INFO={$::errorInfo}"
					set status 0
					$mkv::debug "Making $depend failed, so $target won't be made"
					if { $mkv::p::keep_going } {
						vlog "- although continuing with other targets (-k)"
					} else {
						set failgoal $depend
						break
					}
				}
			}

			vlog "($target) Has prerequisites: $prereq"

			# For prereq, just make sure that they exist.
			foreach p $prereq {
				vlog "Considering $p as prerequisite for $target"

				# This implements "phony prerequisites". 
				if { ![info exists $db_phony($p)] && [file exists $p] } {
					continue
				}

				# Build make tree for targets that:
				# - are real targets and the file doesn't exist
				# - are phony targets

				incr depth
				mkv::p::debug_indent+
				set res [catch {build_make_tree [concat $parentstack $target] $p $target} result]
				mkv::p::debug_indent-
				$mkv::debug "... <--- back at '$target'"
				incr depth -1

				if { $res } {
					$mkv::debug "ERROR: ''$result'' $::errorCode $::errorInfo"
					set status 0
					vlog "Making $p failed, so $target won't be made"
					if { $mkv::p::keep_going } {
						vlog "- although continuing with other targets (-k)"
					} else {
						set failgoal $p
						break
					}
				}
			}
		}

		if { $status == 0 } {
			error "FAILED '$target' at '$failgoal' due to:\n+++ $result"
		}

		# If a phony target doesn't have action, it won't be checked for generic action, too.
		# --- if { [info exists db_phony($target)] && ![info exists db_actions($target)] } {
			# ---         vlog "Target '$target' is phony and has no action - skipping"
			# --- 
			# ---         # Update the need database HERE because pure phony target will be no longer
			# ---         # processed. Not doing it for normal target because this will undergo
			# ---         # further processing.
			# ---         set db_need($whoneedstarget) [lsort -unique [concat $target [pget db_need($whoneedstarget)]]]
			# ---         return
			# --- }

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
			vlog "File '$target' $reason - resolving action for '$target' (@[pwd])"
			if { ![enqueue_target $target $target $whoneedstarget] } {
				error "Action resolution failure for '$target'"
			}
		} elseif { $hasdepends } {
			vlog "Checking if $target is fresh:"
			foreach depend $depends {
				vlog "... against $depend [expr {[info exists db_phony($depend)] ? "(PHONY)":""}]..."
				set stale [is_target_stale $target $depend]
				if { $stale } {
					vlog "File '$target' is stale against '$depend', will be made"
					dict set db_stale($target) $depend 1
					break
				}
			}

			# If a phony target is a PREREQUISITE (not DEPENDENCY) of $target,
			# then it was scheduled to build, however it should be considered
			# not required as a prerequisite for the target, if by DEPENDS check
			# the target isn't considered stale.

			# XXX This is a little bit wrong. There should be somehow recognized
			# as to whether this target is a prerequisite of the parent target.
			# If the parent target was NOT scheduled for execution, this target
			# also shouldn't be, if it's phony.

			if { $stale || [info exists db_phony($target)] } {
				vlog "Resolving make action for $target (stale against $depend)"
				if {![enqueue_target $target $target $whoneedstarget]} {
					error "Action resolution failure for $target"
				}
			} else {
				vlog "File $target is fresh, so won't be made"
				# Good, but it it's fresh, then mark it as done.
				lappend q_done $target
			}
		} else {
			vlog "File $target exists and has no dependencies, so won't be made"
		}
	} result]

	if { $catch } {
		if { $mkv::p::failtarget == "" } {
			set mkv::p::failtarget $failgoal
		}
		# Propagate the result
		return -code $catch $result
	}
	return $result
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

	variable q_pending
	variable q_running
	variable failed
	variable q_done

	set makefile_regenerated no
	set start_target $target ;# The variable is unfortunately widely reused down there.

	# This is only for a case when there's a need to repeat this process.
	while 1 {

		if { [catch {build_make_tree "" $target ""} result] } {
			puts stderr "+++ Rule definition error at '$target'\n+++ $result"
			return false
		}

		# Now look for a pending target as to whether it's $mkv::makefile. When such a target is found,
		# then a different procedure must be undertaken: The build tree must be reduced to only makefile
		# and its dependencies, then the whole process must start again. It must be also checked if the
		# process has updated the makefile really because otherwise this would cause an infinite loop.
		set need_regen [check_makefile_rebuild]

		if { $need_regen && $makefile_regenerated } {
			# Prevent this from happening again and again...
			puts stderr "+++ The rule to regenerate '$mkv::makefile' (starting from '$target') did not rewrite it!"
			puts stderr "+++ Quitting to prevent infinite loop."
			return false
		}

		set makefile_regenerated $need_regen

		vlog "------ Target queue prepared ---- starting action ------"
		$mkv::debug "::: Action queue: [llength $q_pending] pending actions :::"

		foreach e $q_pending {
			lassign $e target ac need
			$mkv::debug " -::- $target:  (need by $need): [string map {"\n" "\\n"} $ac]"
		}
		vlog "--------------------------------------------------------"


		while 1 {

			set channels ""
			foreach target2start [mkv::p::dequeue_action $mkv::p::maxjobs] {
				vlog "--- DEQUEUED/init: $target2start"
				lassign $target2start target action whoneedstarget
				vlog "--- TARGET: $target ACTION: $action PARENT: $whoneedstarget"

				# Add this target to the running targets
				lappend q_running $target
				lappend channels [list $target [mkv::p::pprepare $action]]
			}
			vlog "--- CURRENTLY RUNNING: $q_running"

			if { $channels == "" } {
				break
			}

			pprun ::mkv::p::tracer $channels {{r_res r_ret} {
				upvar ::mkv::p::q_running q_running
				upvar ::mkv::p::q_pending q_pending
				upvar ::mkv::p::q_done q_done
				upvar ::mkv::p::failed failed
				vlog "+++ VBLANK RUNNING: $q_running"
				upvar $r_res res
				upvar $r_ret ret
				$mkv::debug "VBLANK: -- res:"
				foreach {rk rv} $res {
					$mkv::debug "      : $rk -- $rv"
				}
				$mkv::debug "VBLANK: -- ret:"
				foreach {rk rv} $ret {
					$mkv::debug "      : $rk -- $rv"
				}
				set lres [expr [llength $res]/2]
				set lremain [expr {$mkv::p::maxjobs-$lres}]
				$mkv::debug "+++ Channels used: $lres remaining: $lremain"
				if { $lremain < 0 } {
					error "INTERNAL ERROR! (lres $lres > maxjobs $mkv::p::maxjobs, res.size = [llength $res])"
				}

				# If there are no free channels, do nothing.
				if { $lremain == 0 } {
					$mkv::debug "+++ No more free channels - waiting for any process to finish"
					return
				}

				set exit_on_failure 0

				# Check which actions have been finished. Remove them from q_running.
				# We have here just one action
				foreach {deadkey retdb} $ret {
					set target [dict get $retdb target]
					vlog "+++ Found finished target: $target"

					# Remove the target from running
					set q_running [plremove $q_running $target]

					# Check the status
					set failedcmd [dict get $retdb failed]
					if { $failedcmd != "" } {
						vlog "+++ Target '$target' failed on: $failedcmd"

						pluappend failed $target
						if { $mkv::p::keep_going } {
							vlog "+++ ... although continuing on other targets"
						} else {
							set exit_on_failure 1
						}
					}
					dict set done_targets $target 1
					foreach dt [mkv::p::resolve_pure_phony $target] {
						dict set done_targets $dt 1
					}
					set done_target_list [dict keys $done_targets]

					vlog "+++ Adding done target: $done_target_list (for the sake of $target)"

					# Add to done targets (regardless of the status)
					lappend q_done {*}$done_target_list
				}

				# Don't schedule anything if requested to exit on failure.
				if { $exit_on_failure } {
					vlog "+++ NOT SCHEDULING any other targets due to exitting on failure"
					# Clear the pending queue so that the machine stops by starving.
					set q_pending ""
					return
				}

				# This should remove from ret - but in ret there should be only one
				set ret ""

				# Get the next action that can be done
				set channels ""
				foreach target2start [mkv::p::dequeue_action $lremain] {
					vlog "--- DEQUEUED/cont: $target2start"
					lassign $target2start target action whoneedstarget
					lappend channels [list $target [mkv::p::pprepare $action]]
				}
				# Don't schedule anything if empty
				if { $channels != "" } {
					mkv::p::ppupdate res $channels
				}

				vlog "+++ NOW RUNNING: $q_running"

			}}

			if { $q_pending == "" } {
				vlog "+++ No more pending after all running finished - breaking"
				break
			}

			vlog "+++ ALL CHANNELS FREE (occasionally at once) -- repeating the make loop."
		}

		if { $failed != "" } {
			puts stderr "+++ Failed: $failed"
			return false
		}

		if { $q_pending != "" } {
			vlog "--- Q STILL NOT EMPTY: $q_pending"
			puts stderr "+++ Impossible targets:"
			foreach p $q_pending {
				puts stderr "    [lindex $p 0]"
			}
			return false
		}

		if { $makefile_regenerated } {
			puts stderr "+++ Restarting the make process due to rebuilt '$mkv::makefile'"
			reload_makefile
			# Clear the done list
			set q_done ""

			# LOL, after reloading this target might be no longer valid. If so, then you can only
			# say sorry.
			set target $start_target
			variable db_actions
			variable db_phony
			

			if { ![info exists db_actions($target)] && ![info exists db_phony($target)] } {
				puts stderr "+++ After regeneration, '$target' no longer exists. Sorry. Exiting."
				puts stderr "Actions found: [array get db_actions]"
				puts stderr "Phoneys found: [array get db_phony]"
				return false
			}
			# This means that this was running with a queue purged of everything except makefile.
			# So now repeat the analysis and this time run the schedule normally.
			continue
		}

		return true
	}
}

proc submake args {
	set makeexec [mkv::MAKE]

	set opt [list --submake-parentdir [pwd]]
	if { $mkv::p::keep_going } {
		lappend opt -k
	}

	if { $mkv::p::maxjobs > 1 } {
		lappend opt {*}[list -j $mkv::p::maxjobs]
	}
	
	lappend opt {*}$args

	prun {*}$makeexec {*}$opt
}


# This function checks if the given target is pure phony.
# If not, it just returns it. Otherwise it takes its
# dependencies and returns them filtered by this function.
# This function returns a list, possibly also empty!
proc resolve_pure_phony {target} {
	variable db_phony
	variable db_actions
	variable db_depends

	if { [info exists db_phony($target)] && ![info exists db_actions($target)] } {
		set deplist [pget db_depends($target)]
		set deps ""
		foreach d $deplist {
		        lappend deps {*}[resolve_pure_phony $d]
		}
		return $deps
	}

	return $target
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

	# Not supported:
	# $@ and $%: no support for special archive targets.
	# $*: it's weird, not everywhere supported the same way, and strongly discouraged

	set str [string map [list \
			{$@} $target  \
			{$<} [lindex $depends 0]  \
			{$^} [pluniq $depends] \
			{$+} $depends  \
			{$|} [pluniq [getprereq $target]] \
			{$?} [fresher_depends $target $depends]  \
	] $action]
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
	variable db_need

	# Update need information (regardless if the target is done or not)
	set db_need($whoneedstarget) [lsort -unique [concat $target [pget db_need($whoneedstarget)]]]

	$mkv::debug "NEED LIST (after adding need '$target' for '$whoneedstarget':"
	foreach {user needed} [array get db_need] {
		$mkv::debug "$user NEEDS $needed"
	}

	# Check if the target is already done
	if { $target in $q_done } {
		# It's done, so don't try to do it again.
		$mkv::debug "Not enqueuing $target - already done"
		return pass
	}

	# Now check if already enqueued
	set p [lsearch -index 0 $q_pending $target]
	if { $p != -1 } {
		# Already enqueued, so don't enqueue it again
		$mkv::debug "Not enqueuing $target - already enqueued"
		# However, check if $whoneedstarget is among requirers. If not, add it.
		set current_need [lindex $q_pending $p 2]
		if { $whoneedstarget ni $current_need } {
		        $mkv::debug " --- but updating requestors by $whoneedstarget"
		        lappend current_need $whoneedstarget
		        lset q_pending $p 2 $current_need
		}
		return pass
	}

	# Ok, ready to be enqueued
	lappend q_pending [list $target $action $whoneedstarget]
	return pass
}

proc check_makefile_rebuild {} {

	variable q_pending
	variable db_need

	set schedule ""
	set found 0

	set deps [get_all_needs $mkv::makefile]
	foreach pend $q_pending {
		lassign $pend target action whoneedstarget
		# Find a target named $mkv::makefile. Add it to the schedule.
		if { $target == $mkv::makefile } {
			lappend schedule $pend
			set found 1
			continue
		}

		if { $target in $deps } {
			lappend schedule $pend
		}
	}

	if { !$found } {
		return 0
	}

	puts stderr "+++ Makefile '$mkv::makefile' rebuilding detected."
	puts stderr "+++ This will be done now and the process will restart afterwards."

	# Revert the schedule so that the makefile building action falls the last one.
	# This doesn't change much except that the loop will just be walked through once in a natural order.
	set q_pending [lreverse $schedule]

	return 1
}

proc get_all_needs {target} {
	variable db_need

	if { ![info exists db_need($target)] } {
		return ""
	}

	set deps $db_need($target)
	foreach d $deps {
		lappend deps {*}[get_all_needs $d]
	}

	return $deps
}

proc dequeue_action {nrequired} {
	set schedule ""
	set nqueued 0
	variable q_done
	variable q_pending
	variable q_running
	variable failed
	variable keep_going
	variable db_need

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

	# here will be queue items from q_pending that should land in this
	# queue back (should be retried at the next try).
	set stalled ""

	set size [llength $q_pending]
	for {set pos 0} {$pos < $size} {incr pos} {
		set pend [lindex $q_pending $pos]
		lassign $pend target action whoneedstarget

		vlog "+++Dequeue: Considering target: $target"
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

		set need ""
		foreach n [pget db_need($target)] {
		        foreach pp [resolve_pure_phony $n] {
		                # May happen that dependency leads to itself
		                if { $pp != $target } {
		                        lappend need $pp
		                }
		        }
		}

		set alldone 1
		vlog " --- checking if needs for '$target' are satisfied: $need (resolved from: [pget db_need($target)])"
		$mkv::debug "CHECKING '$need' in done:{$q_done} and failed:{$failed}"
		foreach n $need {
		        # Check if all "needs" are already done
		        if { $n in $q_done } {
		                continue
		        }

		        # Check also if it's failed. If so, move this target also to fail
		        if { $n in $failed } {
		                if { !$keep_going } {
		                        vlog " --- found failed target, no -k option -- bailing out"
		                        return ""
		                }

		                # Otherwise just mark this target as failed by dependency
		                vlog " --- Marking target '$target' failed."
		                pluappend failed $target
		        }

		        set alldone 0
		        break
		}

		if { !$alldone } {
		        vlog " --- requires '$n' which is not yet done."
		        # but the target should be considered next time!
		        lappend stalled [list $target $action $whoneedstarget]
		        continue
		}

		if { [string trim $action] == "" } {
		        vlog " --- is an empty target -- marking as done"
		        # This is probably a phony target that just need to fit
		        # well in the dependency and build tree. Mark it done
		        # and do not return it as scheduled.
		        lappend q_done $target

		        # That target won't land in stalled, so it will be
		        # removed from the pending list.

		        # If the target has some failed target in the dependencies,
		        # then it has been moved to failed queue already.
		        continue
		}

		vlog " --- adding '$target' to schedule"

		lappend schedule [list $target $action $whoneedstarget]
		incr nqueued
		if { $nqueued == $nrequired } {
		        vlog " --- added all $nrequired targets to schedule"
		        break
		} else {
		        vlog " --- STILL [expr $nrequired-$nqueued] free channels"
		}
	}

	set q_pending [concat $stalled [lrange $q_pending [expr {$pos+1}] end]]

	# May happen that nothing has been extracted or less have been
	# extracted than it was requested.

	vlog " --- scheduled $nqueued of required $nrequired"
	$mkv::debug "DEQUEUED TO RUN: $schedule"

	$mkv::debug "::: UPDATED Action queue: [llength $q_pending] pending actions :::"

	foreach e $q_pending {
		lassign $e target ac need
		$mkv::debug " -::- $target:  (need by $need): [string map {"\n" "\\n"} $ac]"
	}

	return $schedule
}

# Arguments:
# actual_target - name of target, which is actually being made
# target - name of target which identifies actions to be taken
# whoneedstarget - the target this one is needed for

# If action is performed normally, both targets are the same
# If action is taken as a link to another target, actual_target
# is the target actually built; target is only the indetifier of
# the action, which should be taken.
proc enqueue_target {actual_target target whoneedstarget} {
	set actions [resolve_action $actual_target $target $whoneedstarget]

	# The remaining 'action' is the extracted commandset.
	# Now enqueue the action.
	$mkv::debug "ENQUEUING $actual_target with ACTIONS: {$actions} NEEDED BY: $whoneedstarget"

	if { [catch {enqueue_action $actual_target $actions $whoneedstarget} error] } {
		puts stderr "+++ Error enqueuing '$actual_target': $error"
		return false
	}

	return true
}

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
	set phony false

	if { ![info exists db_actions($target)] } {
		vlog "No specific actions found for '$target' - checking generic actions"
		set generic [find_generic $actual_target]
		if { $generic == "" } {
			if { [info exists db_phony($target)] } {
				vlog "No generic actions, but target is phony - going on with empty action"
				set phony true
			} else {
				puts stderr "+++ No rule to make target '$target'"
				lappend mkv::p::failed $target
				error "File not found/Target not defined: $target"
			}
		} else {
			vlog "Found generic '$generic' applicable for target '$target'"
		}
	}

	set actions ""
	set depends ""
	if { $phony } {
		vlog "NOT Resolving action for a phony target: $target (adding empty action)"
		return ""
	} else {

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
			$mkv::debug "Substitution failed: $result"
			error $result
		}

		vlog "ACTUAL ACTION: $actions"
		#Set special values

		set actual_actions ""

		foreach action $actions {
			set mkv::p::action_performed 1
			# apply standard make options

			set action [pflat $action]

			# Check only for "link" request
			set link ""
			switch -- [string index $action 0] {
				= {
					set link [string trim [string range $action 1 end]]
				}

				! {
					if { [lindex $action 0] == "!link" } {
						set link [lrange $action 1 end]
					}
				}
			}

			# If so, do recursive call to resolve_action to pick up the right target
			if { $link != "" } {
				set linked_target $link
				# XXX handle multiple targets in !link command
				if { ![info exists db_actions($linked_target)] } {
					puts stderr \
					"+++ Can't resolve $linked_target as a link to action"
					lappend mkv::p::failed $target
					return
				}

				# Do substitution before altering target
				set newactions [resolve_action $target $linked_target $whoneedstarget]
				if { $newactions == "" } {
					lappend mkv::p::failed $target
					return
				}
				lappend actual_actions {*}$newactions
				continue
			}

			lappend actual_actions $action
		}

	}

	return $actual_actions
}

# Autoclean automaton

proc rolling_autoclean {rule debug flagged} {

	variable db_depends
	variable db_actions
	variable db_generic
	variable db_phony
	variable db_prereq
	variable db_flags

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

	set excluded 0
	set enforced 0
	if { $flagged != "" } {
		foreach f $flagged {
			set not 0
			if { [string index $f 0] == "-" } {
				set f [string range $f 1 end]
				set not 1
			}

			if { [lsearch [pget db_flags($rule)] $f] == -1 } {
				# Flag not found
				#puts stderr "For '$rule', flag $f NOT found, expected to [expr {$not ?{NOT}:{}}] be found"
				set excluded [expr {$excluded || !$not}]
				set enforced 0
			} else {
				#puts stderr "For '$rule', flag $f FOUND, expected to [expr {$not ?{NOT}:{}}] be found"
				# Flag found, so don't exclude 
				set excluded $not
				set enforced [expr {$enforced || !$not}]
			}
			if { $excluded } {
				break
			}
		}
	}

	if { $deps == "" } {
		if { [info exists db_depends($rule)] && [info exists db_actions($rule)] } {
			if { [info exists db_phony($rule)] } {
				$debug "WON'T DELETE $rule - phony target not being a file"
			} elseif { $excluded } {
				$debug "WON'T DELETE $rule - by flags: $flagged"
			} else {
				$debug "WILL DELETE: $rule - no dependencies, but has a build rule"
				lappend autoclean_candidates $rule
			}
		} else {
			if { $generic != "" } {
				set genshow " (generics: $generic)"
			} else {
				set genshow ""
			}
			if { $enforced } {
				$debug "WOULD DELETE: $rule - no dependencies$genshow, but enforced by flags: $flagged"
				lappend autoclean_candidates $rule
			} else {
				$debug "WON'T DELETE: $rule - no dependencies$genshow"
			}
		}
	} else {
		if { ![info exists db_actions($rule)] && $generic == "" } {
			$debug "WON'T DELETE: $rule - no action (phony rule)"
		} elseif { $excluded } {
			$debug "WON'T DELETE $rule - by flags: $flagged"
		} else {
			$debug "WILL DELETE: $rule (built from $deps)"
			lappend autoclean_candidates $rule
		}
	}

	# Extract and filter out dependency files
	set depfiles ""
	set deps [InterpretDepends $rule $deps depfiles]

	incr ::gg_debug_indent
	foreach dep $deps {
		lappend autoclean_candidates {*}[rolling_autoclean $dep $debug $flagged]
	}
	if { $depfiles != "" } {
		$debug "WILL DELETE DEPFILE: $depfiles"
		lappend autoclean_candidates {*}$depfiles
	}
	incr ::gg_debug_indent -1
	return [lsort -decreasing -unique $autoclean_candidates]
}

proc is-dir-nonempty d {
	set dircon [glob -nocomplain -tails -dir $d * .*]
	# Linear searching because normally . and .. appear at the very beginning
	set rem 0
	set dirconout ""
	for {set i 0} {$i < [llength $dircon]} {incr i} {
		set ed [lindex $dircon $i]
		if {$rem == 2} {
			#Stop checking, copy just one to mark nonempty
			lappend dirconout $ed
			break
		}
		if {$ed in ". .." } {
			incr rem
			continue
		}
		lappend dirconout $ed
		break ;# we just need one other than . .. to prove nonempty.
	}

	return [expr {$dirconout != ""}]
}

proc autoclean {rule args} {
	set mkv::p::gg_debug_indent 0
	set ac [rolling_autoclean $rule $mkv::debug $args]
	if { $ac != "" } {
		puts stderr "Autoclean deletes: $ac"

		foreach d $ac {
			if { [file isdirectory $d] } {
				if { [is-dir-nonempty $d] } {
					puts stderr "*** Leaving directory '$d': not empty. Deletion MAY be unwanted."
					continue
				}
			}

			if { [catch {file delete {*}$d} result] } {
				puts stderr "*** ERROR: can't delete '$d': $result"
			}
		}

	}
}

proc autoclean-test {rule args} {
	set mkv::p::gg_debug_indent 0
	set ac [rolling_autoclean $rule debug $args]
	puts stderr "Autoclean would delete: $ac"
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


proc finished_program {var ix op} {
	puts stderr "agmake\[$mkv::p::dirdepth\]: Leaving directory '[file normalize $::mkv::directory]'"
	incr mkv::p::dirdepth -1
}

set public_export [puncomment {

	# Main make facilities
	phony
	rule
	rules
	gendep
	dep-rule
	make
	submake
	setflags
	getflags

	# Logging
	vlog

	# Utility functions
	dict:at
	dict:filterkey
	autoclean
	autoclean-test
}]

# Add utilities from mkv.p.utilities.tcl
lappend public_export {*}$public_export_util {*}$public_export_builtin

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

variable target
variable targets

}


package provide make 0.5

namespace import {*}$mkv::p::public_import

# Rest of the file is interactive.

proc main argv {

	set help 0

	set g_optargs {
		--help *help
		-h *help
		-C makefiledir
		-d *display_debug
		-f makefile
		-k *keep_going
		-j jobs
		-x tcl_command
		-v *verbose
		--submake-parentdir parentdir
	}

	set keep_going 0
	set display_debug 0
	set verbose 0
	set makefile {}
	set help 0
	set makefiledir .
	set jobs 1
	set parentdir ""
	set tcl_command ""

	lassign [process-options $argv $g_optargs] cmd_args cmd_variables

	if { $help } {
		puts stderr "Options:"
		foreach {opt arg} $g_optargs {
		        puts stderr [format "  %-8s %s" $opt: $arg]
		}
		return 1
	}

	unset help
	set mkv::p::keep_going $keep_going
	set mkv::makefile $makefile
	set mkv::debug_on $display_debug
	set mkv::p::verbose $verbose
	set mkv::directory $makefiledir
	set mkv::targets $cmd_args

	if { $jobs == "j" } {
		set jobs [mkv::p::number-cores]
	}

	if { ![string is integer $jobs] || $jobs < 1 } {
		error "Invalid value for -j '$jobs': allowed is 'j' or positive integer"
	}

	set mkv::p::maxjobs $jobs

	if { $display_debug } {
		set mkv::debug mkv::p::debug
	}

	foreach {name value} $cmd_variables {
		#puts "Setting $name to $value"
		uplevel #0 set $name $value
	}

# --- SET DIRECTORY - before looking for makefile ---
	set wd [pwd]
	set makefiledir [file normalize $makefiledir]
	if { $parentdir != "" } {
		incr mkv::p::dirdepth
		puts stderr "agmake\[$mkv::p::dirdepth\]: Entering directory '$makefiledir'"
		trace variable parentdir u ::mkv::p::finished_program
	}
	cd $makefiledir

	if { $mkv::makefile == "" } {
		foreach f $mkv::p::default_makefiles {
			if { [file exists $f] } {
				set mkv::makefile $f
				break
			}
		}

		if { $mkv::makefile == "" } {
			puts stderr "No makefile found among the following possible names:"
			puts stderr [join $mkv::p::default_makefiles " "]
			error "No makefile found"
		}
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
	uplevel #0 source $mkv::makefile

	$mkv::debug "SUMMARY DATABASE (direct targets):"
	foreach n [array names mkv::p::db_depends] {
		$mkv::debug "$n: $mkv::p::db_depends($n) {[pget mkv::p::db_actions($n)]}"
	}

	$mkv::debug "Executing statements"

	if { $tcl_command != "" } {
		# There's just a Tcl command to execute.
		# Execute it and say it's done.
		set mkv::p::action_performed 1
		uplevel #0 $tcl_command
	} elseif {$cmd_args == ""} {
		set cmd_args $mkv::p::first_rule
	}

	# Here cmd_args can only be empty if there was an -x command.
	# So, with -x "some tcl command" and no target will result in cmd_args == "".
	if { $cmd_args != "" } {
		set res1 true
		if { [set xc [catch {
			$mkv::debug "WILL MAKE: $cmd_args STARTING FROM $mkv::p::first_rule"
			foreach tar $cmd_args {
				$mkv::debug "Applying make to '$tar'"
				if { [catch {make $tar} res1] } {
					$mkv::debug "FAILURE: $res1 $::errorCode $::errorInfo"
					#puts stderr "FAILURE: $::errorInfo"
					error $res1 $::errorInfo
				} elseif { !$res1 } {
					# XXX This was displaying mkv::p::failtarget, but it was always empty.
					# Track this down whether it should be still that one, but properly set.
					error "Stopped on '$mkv::p::failed' target"
				}

			}
		} result]] || !$res1} {

			puts stderr "+++ Target '$tar' failed: $result"
			$mkv::debug "+++ ($::errorInfo)"
			#puts stderr "+++ ($::errorInfo)"
			return 1
		}
	}

	if { !$mkv::p::action_performed } {
		puts stderr "+++ Nothing to be done for: $cmd_args"
	}

	cd $wd

	return 0
}

if { !$tcl_interactive } {
	set er [catch {main $argv} result]
	#puts stderr "--- Interactive run finished: exception: $er result: $result"
	if { $er } {
		puts stderr "+++ERROR: $result"
		exit 1
	}
	# Whenever result isn't translateable to a nonzero integer, coerce it to 0.
	if { ![string is integer $result] } {
		puts stderr "+++ (weird result: $result)"
		set result 0
	}

	exit $result
}

;# end of interactive actions
