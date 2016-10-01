## Silvercat check facilities

# Disclaimer

This is a description of a future feature, a slight plan to create automatic system detection.

# Basic statements

All these check functions have important common treats:

1. All these functions must perform some test that is expected to pass or fail.
1. With default settings the check goes on if the test passed and causes termination if failed.
1. With additional options you can control the behavior of what happens in case of pass and failure

Note that this behavior will be referred to as "checks if", which means that by default it would interrupt if the check failed.

There are three groups of commands:

1. Simple commands that have ag-check- prefix, which just do a simple condition check or specialized checking functions.
1. Helper functions that are just commonly used when writing checks.
1. The "checkers" (modules of ag-check)

# Common options

All check commands accept the following options:

- `-failmsg <message text>` - when failed, it prints the failure message
- `-then <code>` - if the check has succeeded, execute `<code>` (turn off interrupt on failure)
- `-else <code>` - if the check has failed, execute `<code>` (instead of interrupting)
- `-key <keyname>` - don't interrupt on failure, store the result as 1 or 0 under the key `have_<keyname>`
- `-retry <args...>` - if the check failed, do the checks once more with different set of parameters (the list of parameters depends on parameters of the checker)
- `-yes <message>` - set information message that will be printed instead of standard "yes" when the test passed
- `-no <message>` - set information message that will be printed instead of standard "no" when the test failed

Every checker has the check info message as the first argument.
This message will be printed as the checker message.
If the string is empty, no message will be printed at all.


# Simple checker commands

These things behave as usual checked (interrupts on failure), but they are not
typical checkers, that is, they don't print any message 

    ag-check-file <path>

This command checks if the given file exists (may be also a directory). 

    ag-check-option <option-name> <help-text> [options]

This command checks if a command-line option `--<option-name>` is present in
the arguments.  This command accepts the usual -then and -else options, but it
doesn't interrupt on failure.  If the option is present, the name of the option
is available as $0 and the arguments as following $1, $2 etc. For one argument,
both `--option=value` and `--option values...` are the same. The arguments are
available only for -then block (for -else block only $0). Additionally, the $#
is replaced with the number of arguments and $@ with the whole list of
arguments.

When no -then or -else blocks are given, the option is remembered in the "option" entry
of the configuration. Normally option is boolean, unless you use the -default option
for this command, in which case this is the default option. It is an error to pass a
command line with option that has a non-boolean value, when there was no -default option.
Passing just the option without argument makes it true by default.

In other words, if -default is given, then it is treated as a default option value, and
then any kind of text, also empty, is accepted. Otherwise the option is boolean, it's
false by default, and when present it's expected to be either a standard Tcl boolean
value or an empty string, which is interpreted as true.

# The ag-check command and checkers

The general syntax of ag-check command is:

    ag-check LANGUAGE CHECKER ARGS...

Where ARGS... may also include standard options as above.

Checkers are free to define whatever other options it uses.
The standard options will be eaten up by ag-check command,
the unrecognized options will be sent to the checker.

When building blocks with alternatives and optionals, you have to remember
that:

1. By default, every checker causes termination on failure. It means that
if you place two command calls one after another, where the first one is
going to terminate on failure, the second one will be only executed if the
preceding command has succeeded!
1. If you specify -then option, it clears the terminative character of the
checker command. Use -then only if THIS checking is optional and your program
can still continue, maybe with a bit decreased functionality.


[//]: # vim: ft=markdown
