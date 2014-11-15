# README #

Silvercat aims to be a high-level build system - a tool of similar purpose as autotools, cmake, qmake, scons or Jam.

It's entirely written in Tcl and Tcl is also used as a build definition file interpreter.

Like almost all tools of that kind, it's split in two parts: first is a tool that define the direct rules, where most likely the POSIX "make" tool is used, and the high-level build system itself just generates Makefile.

In case of this system, it has its own version of "make" tool (make.tcl), with syntax reminding the syntax of "make" tool, although defined as a set of additional Tcl commands.

### What is this repository for? ###

This repository contains:

* In bin/ directory
     * The make.tcl tool (the rule execution tool)
     * The ag.tcl tool (the rule generator tool)
* In examples/ directory
    * The example for Makefile.tcl
    * The example for Makefile.ag.tcl

### How do I get set up? ###

In the current state there are no direct instructions of installing. The ag.tcl is also split into smaller parts, all of them residing in the same bin/ directory. The only thing you can do at best is to copy everything from bin/ directory to some directory for which you have PATH set.

The Tcl interpreter version 8.5 is required to run these scripts.


### Contribution guidelines ###

As the project is in early development stage, there are no strict rules set up for contribution. If you wish to contribute, please do two things:

* Contact me (admin)
* Understand how the project really works

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact

