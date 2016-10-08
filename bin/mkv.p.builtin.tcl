
variable depth
set depth 0
variable verbose
set verbose 0
variable generated
set generated ""

variable default_makefiles {
	Silvermakefile
	silvermakefile
	AGMakefile
	agmakefile
	Makefile.tcl
	makefile.tcl
}

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

# This function recognizes if the dependency specification is in the
# "make" format or in "Tclmake" format (only the source files and include
# files in one line). If "make" format is found, translate it to Tclmake format.
proc TranslateDeps {indeps} {
	# deps that don't have : at the end of the first word
	set first [lindex [split $indeps " "] 0]
	if { [string index $first end] != ":" } {
		return $indeps
	}

	# First, remove these stupid line breaks
	set deps [string map {"\\\n" " "} $indeps]

	# Second, delete the first word which is the name of the target
	# (we'll have the name set explicitly)
	return [lrange $deps 1 end]
}

variable gg_debug_indent 0
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



set public_export_builtin  {
	vlog verbose depth
}

namespace export {*}$public_export_builtin
