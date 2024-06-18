
# Override any default or detectable compilers.
set cc [pget CC]
if { $cc == "" } {
	set cc [pget env(CC)]
}

if { $cc != "" } {
	set cmd [auto_execok $cc]
	if { $cmd == "" } {
		error "Command in CC '$cc' is not executable"
	}
	catch { exec $cc -v |& grep {gcc version} } gccver
	set gccver [lindex $gccver 2]
	if { [pver $gccver < 4.7] } {
		error "Gcc $gccver is too old."
	}

	# Succeeded, so set also cxx.
	set cxx [pget CXX]
	if { $cxx == "" } {
		set cxx [pget env(CXX)]
	}
	if { $cxx == "" } {
		# Then try to use cc and change gcc to g++
		set cxx [string map {gcc g++ cc c++} $cc]
	}

	set GCCBASE $cc
	set GXXBASE $cxx

	puts stderr "PROFILE: using custom based on CC=$cc"

	source ag/ag-profile-gcc-custom.tcl
	ag-profile gcc-custom

# For CentOS 6 use special gcc-new profile.
# Otherwise use the "current best default" gcc-native profile.
} elseif { $tcl_platform(os) == "Darwin" || [phas USE_CLANG] } {

	puts stderr "PROFILE: using clang"
	# use clang on Mac.
	ag-profile clang-native
	set GCCBASE clang
	set GXXBASE clang++
} else {

	puts stderr "PROFILE: using gcc"
	# Use the default profile.
	ag-profile gcc-native
	set GCCBASE gcc
	set GXXBASE g++
}

# vim: ft=tclmake
