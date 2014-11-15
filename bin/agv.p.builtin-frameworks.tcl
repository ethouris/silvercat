# Frameworks

# This is a set of hooks done by the following rule:

# namespace fw
# namespace <ID name of the framework>
# procedures:
#  - prepare: the database wasn't yet processed at all
#  - compile-pre: the sources have been reviewed and dependencies for existing ones generated or confirmed
#  - compile-post: after compile commands and rules have been generated for all sources
#  - complete: after all steps for given target have been completed (linker rules, in this case)

namespace eval fw {
	namespace eval pkg-config {
		proc prepare {target} {
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
}


