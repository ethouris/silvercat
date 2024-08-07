The make replacement tool (agmake)
==================================

Requires Makefile.tcl build configuration. This works merely like the
usual 'make' tool. The example simple Makefile:

    CC=gcc

    hello.o: hello.c hello.h
        $(CC) -o $@ $<


can be written in Makefile.tcl as:


    set CC gcc

    rule hello.o hello.c hello.h {
        $CC -o $@ $<
    }

It has also several more advanced facilities, and it covers practically
all important features of the 'make' tool, except default rules.


The silvercat tool (agcat)
===========================

High level build system.

The simplest definition for a program consisting of two source files is:


    ag myprog -type program -sources source1.c source2.cpp


Two modes are currently supported:

* agcat genrules - Generate Makefile.tcl with synthesized rules
* agcat make - Synthesize rules and execute them in place


The project is currently hosted on Gitlab, here's the Wiki homepage:

https://gitlab.com/silvercat/silvercat/wikis/home

