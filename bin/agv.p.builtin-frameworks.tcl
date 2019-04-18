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

			set tparts [file split $namev]

			# Package can be also specified as "NAME-VER/LIBSPEC".
			# By default we take the dynamic library, but static can be also specified

			if { [llength $tparts] == 1 } {
				set libspec shared
			} elseif { [llength $tparts] == 2 } {
				lassign $tparts namev libspec
				if { $libspec ni {static shared} } {
					error "pkg-config fw: unknown libspec for pkg '$namev':'$libspec' (use static or shared)"
				}
			} else {
				error "pkg-config fw: incorrect pkg name specification: $namev"
			}

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
					return [list $namev $v $libspec]
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
						if { [regexp "$name-\[0-9\]" $p] } {
							lappend pkgs $p
						}
					}

					if { $pkgs == "" } {
						$::g_debug "pkg-config: no package matches $name"
						return
					}

					foreach p $pkgs {
						set v [exec $pkg_config --modversion $p]
						if { [package vcompare $v $version] != -1 } {
							puts stderr "WARNING: pkg-config doesn't know neiter '$name' nor '$namev'."
							puts stderr "WARNING: Found '$p', which seems to have a similar name and required minimum version."
							return [list $p $v $libspec]
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
					return [list $name $v $libspec]
				}

				# This means: package found, but not in the required version.
				# puts "pkg-config FindPackage: $namev -$v $libspec"
				return [list $name "-$v" $libspec]
			} else {
				$::g_debug "pkg-config with no version for '$namev':"
				$::g_debug "PKG_CONFIG_PATH=[pget ::env(PKG_CONFIG_PATH)]"
				$::g_debug "COMMAND: $pkg_config --exists $namev"
				# So just find the package by name without checking for version
				if { [catch {exec $pkg_config --exists $namev} erp] } {
					$::g_debug "... ERROR: $erp"
					return
				}

				return [list $namev "" $libspec]
			}
		}

		proc ExtractPackageFlags {r_db packages} {
			variable pkg_config
			upvar $r_db db

			set confirmed ""
			vlog "Packages: $packages"
			foreach p $packages {
				lassign [FindPackage $p] name version spec
				#puts "FindPackage: $name-$version ($spec)"
				$::g_debug "pkg-config:FindPackage returned name '$name' version '$version' spec '$spec'"
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

				set ifstatic ""
				if {$spec == "static"} {
					set ifstatic "--static"
				}

				$::g_debug "Calling pc: $pkg_config --libs --cflags $ifstatic $name"
				set ldflags [exec $pkg_config --libs {*}$ifstatic $name]
				set cflags [exec $pkg_config --cflags {*}$ifstatic $name]

				if { $spec == "static" } {
					# Fix the -l flag so that the static library name is enforced
					if { [dict exists $db language] } {
						set pfl $agv::profile([dict get $db language])
						set pfd $agv::profile(default)
						set lflag [dict get $pfl link_lflag]
						set ix [lsearch $ldflags $lflag*]
						if { $ix == -1 } {
							puts stderr "NOTE: library spec for $name has no $lflag flag"
						} else {
							set libname [pdip [dict get $pfd form:archive] $name]
							lset ldflags $ix ${lflag}:$libname
							puts stderr "NOTE: library spec for $name changed to ${lflag}:$libname"
						}
					} else {
						puts stderr "NOTE: profile provides no -lang key, keeping orig library name"
					}
				}

				vlog "PACKAGE: $name: cflags='$cflags' ldflags='$ldflags'"

				dict lappend db ldflags {*}$ldflags
				dict lappend db cflags {*}$cflags
			}

			return $confirmed
		}

		proc compile-pre {target args} {
			vlog "Running pkg-config framework for '$target'"
			# Take the package name, extract the library
			# parameters, apply to the database.
			set db $::agv::target($target)

			set packages [dict:at $db packages]
			set lang [dict:at $db language]
			if { $lang == "" } {
				puts stderr "NOTE: pkg-config can't propagate global package: unknown language for $target"
			} else {
				set globalpkg [dict:at $agv::profile($lang) packages]
				if { $globalpkg != "" } {
					vlog "+++--- pkg-config: propagating packages from profile: $globalpkg"
					lappend packages {*}$globalpkg
				}
			}

			set confirmed [ExtractPackageFlags db $packages]

			set newpkg ""

			foreach p $packages {
				if { $p ni $confirmed } {
					lappend newpkg $p
				}
			}
			$::g_debug "Confirmed packages: $confirmed"
			$::g_debug "Unrecognized packages: $newpkg"

			if { $newpkg != "" } {
				puts stderr "ERROR: Packages not found: $newpkg"
				error "Cannot configure due to lacking packages"
			}

			dict set db packages ""

			# Write back the database
			set ::agv::target($target) $db
		}
	}
}


