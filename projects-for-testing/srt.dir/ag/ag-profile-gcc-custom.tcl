
# Temporarily use gcc-new profile with homecooked gcc for haivision
pinit GXXBASE "g++"
pinit GCCBASE "gcc"


# This is a bit "hacking" way of adding profiles.
# There could be a nice way to do that by e.g.
# ag-profile-define profile-name {...contents...}.

set gcc_new_profile [subst {
	default {
		depopt "-MMD -MF "
	}
	c++ {
		compile "$GXXBASE -c"
			link "$GXXBASE"
			linkdl "$GXXBASE -shared"
			gendep "$GXXBASE -MM"
			depspec auto
	}

	c {
		compile "$GCCBASE -c"
			link "$GCCBASE"
			linkdl "$GCCBASE -shared"
			gendep "$GCCBASE -MM"
			depspec auto
	}
}]

# Merge it with gcc-native profile (builtin)
set merged_gcc_profile [dict get $agv::p::profiles gcc-native]
foreach {key val} $gcc_new_profile {
	dict set merged_gcc_profile $key [dict merge [dict get $merged_gcc_profile $key] [pget gcc_new_profile.$key]]
}

lappend agv::p::profiles gcc-custom $merged_gcc_profile

#####################################################################
# The above should be replaced by:
#ag-define-stock-profile gcc-new gcc {c++ g++-4.9 c gcc-4.9}
# or
#ag-define-profile gcc-new gcc-native { default { ... } c++ { ... } c { ... } }
