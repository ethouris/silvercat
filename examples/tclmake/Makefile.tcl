# This is a standard general-purpose makefile for a flail-simple program
# consisting of more than one source file.

# Makefile.tcl is the default name. Another name can be used with -f option.

# The structure looks almost exactly like in the standard Makefile.
# The most basic command is "rule" and it results in the standard
# Makefile's statement: target: depend1 depend2...\n\tcommand to build target

# The 'phony' command is like rule, it just sets the target as phony
# (not connected to a physical file) and does not accept action definitions.
# You can also first define a "rule" and make it "phony" later by just
# saying "phony TARGET".

set mkv::p::verbose 1

# The rule, as in standard Makefile, is that the first found rule is the
# default one. It's usually called 'all'.
phony all answer

# Ok, so here we go with the rule to make the target.
# And no, in contrast to Makefile, the tab before the command isn't required :P
rule answer obj/file1.o obj/file2.o {
	# As you can see, you can use the symbolic replacement known from Makefile.
	# Well, treat this as an extension to Tcl; actually Tcl doesn't try to
	# expand $, if not followed by [a-zA-Z0-9_], so this is just a text.
	[CXX] -o $@ $^
}

rule obj {
	mkdir obj
}

# This is a generic rule - surprise!
# This rule applies to producing anything that matches *.o.
# Actually this rule makes sense in this case because each *.cc
# file includes file.h - this kind of generic rule is not too universal then.
rule obj/*.o *.cc file.h | obj {
	[CXX] -c $< -o $@
}

# pdef can define a delayed definition. Expand with [CXX].
pdef CXX g++


# No need to worry if your clean is properly defined and doesn't leave
# any garbage. The 'autoclean' command provided by the make tool will
# delete every file, for which there's a rule to build, starting from
# given target.
rule clean {
	!tcl autoclean answer
}

rule cleantest {
	!tcl autoclean-test answer
}

# Unblock this, if you want to see how the make.tcl tool works.
# Alternatively, use the -v option.
#set mkv::verbose 1
