# README #

Silvercat aims to be a high-level build system - a tool of similar purpose as autotools, cmake, qmake, scons, Jam, or gn.

It's entirely written in Tcl and Tcl is also used as a build definition file interpreter.

Like almost all tools of that kind, it's split in two parts: first is a tool that define the direct rules, where most likely the POSIX "make" tool is used, and the high-level build system itself just generates Makefile.

In case of this system, it has its own version of "make" tool (agmake), with syntax reminding the syntax of "make" tool, although defined as a set of additional Tcl commands.

### What is this repository for? ###

This repository contains:

* In bin/ directory
     * The agmake tool (the rule execution tool)
     * The agcat tool (the rule generator tool)
	 * Some "library files" used by them both
* In examples/ directory
    * The example for Makefile.tcl
    * The example for Makefile.ag.tcl

### How do I get set up? ###

You need Tcl 8.5 to run these scripts.

To run this project you need some directory with executables that is registered in your PATH environment variable.

There are two methods to install this tool:

* Simple install. Go to `bin/` directory and copy all files into a directory with executable files of your choice.
* Upgradable install. Works only on system that supports symbolic links. Run the `INSTALL_ALL.tcl` script.
The argument is (like above) that executable directory of your choice. This will make symbolic links to scripts
located in `bin/` directory here. Every time when you'd like to upgrade, just go to the repository location,
issue `git pull`, then run the `INSTALL_ALL.tcl` script again (to make sure that any new scripts that might
have been added anew are also linked).



### Contribution guidelines ###

As the project is in early development stage, there are no strict rules set up for contribution. If you wish to contribute, please do two things:

* Contact me (admin)
* Understand how the project really works

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact

