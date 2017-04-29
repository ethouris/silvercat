#!/bin/bash
# Tcl would skip this line \
if [[ ! -x `type -p tclsh` ]]; then echo "Tcl 8.5 and Silvercat are required to run this script"; exit 1; fi; exec tclsh "$0" "$@"

if { [catch {package require ag 1.0} problem] } {
	puts stderr "You need Silvercat 1.0 to configure and build this package."
	puts stderr "You can download it from https://gitlab.com/silvercat"
	exit 1
}

source [file dirname [info script]]/configure.ag.sack

