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

		# XXX To be modified by a config db entry!
		variable pkg_config pkg-config
		variable pkg_config_cache ""

		# This procedure finds the package according to the following
		# convention:
		# 1. if "-[0-9]" phrase was found in the name, it's considered version. This version is extracted
		# 2. The package is first tried to be found with the original name-version statement anyway
		# 3. If this failed, the search continues with the original name this time, but the minumum
		# version is ensured by comparing it using [package vcompare].
		proc FindPackage {namev} {
			variable pkg_config
			variable pkg_config_cache

			if { [regexp -indices -- {-([0-9].*)$} $namev pos] } {
				set p [lindex $pos 0]
				set version [string range $namev $p+1 end]
				set name [string range $namev 0 $p-1]
				$::g_debug "pkg-config: $namev - detected name '$name' and version '$version'"

				# Try to find the name-version first anyway
				if { ![catch {exec $pkg_config --exists $namev}] } {
					# We found this package. Get the version information just for info
					set v [exec $pkg_config --modversion $namev]
					$::g_debug "pkg-config: found as $namev"
					return [list $namev $v]
				}

				# Well, we haven't found the package with that name-version,
				# so check just the name.

				if { [catch {exec $pkg_config --exists $name}] } {
					# Ok, this is crazy, but let's just try to search
					# all names matching the given name with added version.
					$::g_debug "pkg-config: not found as $namev nor $namev - reviewing all packages..."
					set all_pkg_lines $pkg_config_cache
					if { $all_pkg_lines == "" } {
						set all_pkg_lines [exec $pkg_config --list-all]
						set pkg_config_cache $all_pkg_lines
					}
					set pkgs ""
					foreach ln $all_pkg_lines {
						set p [lindex $ln 0]
						if { [regexp "$name-\[0-9\]" $p } {
							lappend pkgs $p
						}
					}

					if { $pkg == "" } {
						$::g_debug "pkg-config: no package matches $name"
						return
					}

					foreach p $pkgs {
						set v [exec $pkg_config --modversion $p]
						if { [package vcompare $v $version] != -1 } {
							puts stderr "WARNING: pkg-config doesn't know neiter '$name' nor '$namev'."
							puts stderr "WARNING: Found '$p', which seems to have a similar name and required minimum version."
							return [list $p $v]
						}
					}

					$::g_debug "pkg-config: version $version not found for '$name' identified as: $pkgs"
					return
				}

				# Ok, let's state then that we found this by the name, 
				# so only make sure about the version

				set v [exec $pkg_config --modversion $name]
				if { [package vcompare $v $version] } {
					$::g_debug "pkg-config: found '$name' version '$v'"
					return [list $name $v]
				}

				# This means: package found, but not in the required version.
				return [list $name "-$v"]
			} else {
				# So just find the package by name without checking for version
				if { [catch {exec $pkg_config --exists $namev}] } {
					return
				}

				return $namev
			}
		}

		proc prepare {target} {
			variable pkg_config

			vlog "Running pkg-config framework for '$target'"
			# Take the package name, extract the library
			# parameters, apply to the database.
			set db $::agv::target($target)

			set packages [dict:at $db packages]
			set confirmed ""

			vlog "Packages: $packages"
			foreach p $packages {
				lassign [FindPackage $p] name version
				$::g_debug "pkg-config:FindPackage returned name '$name' version '$version'"
				if { [string index $version 0] == "-" } {
					set v [string range $version 1 end]
					$::g_debug "NOTE: pkg-config found package '$name', but with outdated version $v"
					continue
				}

				if { $name == "" } {
					$::g_debug "NOTE: pkg-config found no package named '$p' nor '$name'"
					continue
				}

				# Package found. Add to the confirmed list.
				lappend confirmed $p

				set ldflags [exec $pkg_config --libs $name]
				set cflags [exec $pkg_config --cflags $name]

				vlog "Data for $name: cflags='$cflags' ldflags='$ldflags'"

				dict lappend db ldflags {*}$ldflags
				dict lappend db cflags {*}$cflags
			}

			set newpkg ""

			foreach p $packages {
				if { $p ni $confirmed } {
					lappend newpkg $p
				}
			}
			$::g_debug "Confirmed packages: $confirmed"
			$::g_debug "Unrecognized packages: $newpkg"

			# Keep the list of not found packages
			dict set db packages $newpkg

			# Write back the database
			set ::agv::target($target) $db
		}

		proc compile-pre {target args} {
			# This is only sanity check.
			# If all packages were found, the packages key must be empty.
			set pkg [dict:at $::agv::target($target) packages]
			if { $pkg != "" } {
				puts stderr "ERROR: Packages not found: $pkg"
				error "Cannot configure due to lacking packages"
			}
		}
	}
}


