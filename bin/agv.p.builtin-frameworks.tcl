# Frameworks
namespace eval fw {
	proc pkg-config {target} {
		vlog "Running pkg-config framework for '$target'"
		# Take the package name, extract the library
		# parameters, apply to the database.
		set db $::agv::target($target)

		set packages [dict:at $db packages]
		vlog "Packages: $packages"
		foreach p $packages {
			if { [catch {exec pkg-config --exists $p}] } {
				error "Package not found: $p"
			}
			set ldflags [exec pkg-config --libs $p]
			set cflags [exec pkg-config --cflags $p]

			vlog "Data for $p: cflags='$cflags' ldflags='$ldflags'"

			dict lappend db ldflags {*}$ldflags
			dict lappend db cflags {*}$cflags
		}

		# Write back the database
		set ::agv::target($target) $db
	}
}


