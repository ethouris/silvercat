#!/bin/bash
# Check and run with tclsh: \
if [[ -z "`type -p tclsh`" ]]; then echo You need tcl 8.5 to run this install and some of the tools.; exit 1; fi; exec tclsh "$0" "$@"

if { $argv == "" } {
	puts "Usage: INSTALL_ALL.sh <bin_prefix>"
	puts "(creates a symbolic link to all tools into the location of the repository)"
	exit 1
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

set prefix [lindex $argv 0]

if { ![file exists $prefix] } {
	puts "Prefix directory doesn't exist: $prefix"
	exit 1
}

set stack ""
while { [file type $prefix] == "link" } {
	lappend stack $prefix

	set WD [pwd]
	cd [file dirname $prefix]
	set fn [file tail $prefix]
	set prefix [file normalize [file readlink $fn]]
	cd $WD

	if { $prefix in $stack } {
		error "Prefix is a self-recursive link. Please supply a directory or a link to a directory."
	}
}

if { ![file isdirectory $prefix] } {
	puts "Prefix is not a directory: $prefix"
	exit 1
}


set TOOLS [glob {[a-z]*}]
set WD [pwd]

cd $prefix

set ninstalled 0
set nuptodate 0
set noverwritten 0
set ndenied 0

# This procedure returns the same as [file type] except
# if the file doesn't exist, returns "none" instead of throwing an error.
proc file-type-nocomplain tool {
	if { [catch {file type $tool} ft] } {
		return
	}
	return $ft
}

foreach tool $TOOLS {

	set path [file join $WD $tool]
	set tarpath [prelocate $path [pwd]]

	set type [file-type-nocomplain $tool]
	if { $type != "" } {

		# Check if this is a symbolic link that points to a correct location.
		# If so, silently ignore it.
		if { $type == "link" } {
			set link [file readlink $tool]
			if { $link == $tarpath } {
				incr nuptodate
				continue
			}
		}

		puts -nonewline "ERROR: the '$tool' tool ($type) already exists in the target directory. Overwrite? (y/N) "
		flush stdout
		set ans [gets stdin]
		if { $ans == "" } {
			set ans n
		}

		set ans [string tolower $ans]
		if { [string index $ans 0] == "y" } {
			file delete $tool
			incr noverwritten
		} else {
			incr ndenied
		}
	} else {
		incr ninstalled
	}

	file link -s $tool $tarpath
}

puts "INSTALLED $ninstalled files. $nuptodate were up-to-date, $noverwritten overwritten, $ndenied denied to overwrite."

# set ft=tcl
