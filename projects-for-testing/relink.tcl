#!/usr/bin/tclsh

if {$argv == ""} {
	puts "Usage: [file tail $argv0] <project-name>"
	exit 1
}

set proj [lindex $argv 0]

set project_dir "${proj}.sub"
set attach_dir "${proj}.dir"

foreach d [list $project_dir $attach_dir] {
	if {![file exists $d] || ![file isdir $d]} {
		error "Check existence: $d"
	}
}


proc strip_path {filename {n 1}} {
	set parts [file split $filename]
	set ln [llength $parts]
	if {$ln < $n - 1} {
		return
	}

	return [file join {*}[lrange $parts $n end]]
}

set filelist [exec find $attach_dir -type f]

foreach f $filelist {
	#puts $f
	set srcpath [strip_path $f]
	set lname [file join $project_dir $srcpath]
	set ndirs [llength [file split $srcpath]]

	set target [file join {*}[lrepeat $ndirs ..] $f]
	puts "$lname -> $target"
	set base [file dirname $lname]
	if {![file exists $base]} {
		file mkdir $base
	}
	if {[file exists $lname]} {
		puts "FILE '$lname' exists as [file type $lname]"
	} else {
		file link -s $lname $target
	}
}




