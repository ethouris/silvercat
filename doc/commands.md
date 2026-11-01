
Commands in Silvercat
=====================

## `ag`

Usage:
```
ag <target> <keyspecs...>
```

Create or modify target properties.

The first call must specify `-type`. Further calls may add or modify more
properties.

## `ag-profile`

Usage:
```
ag-profile <profilespec> <keyspecs...>

ag-profile <profilename>
```

If there are at least two arguments, the first form is considered, and
the `<profilespec>` argument can be:

* `general` - sets the option applicable for every target (if propagated)
* A single language name or a list of languages in { ... }, for example
   * c++
   * {c c++}

Options are applicable to every target matching the specification (all
targets if `general` or matching the specified language).

If the call is with only one argument, `<profilename>`, it loads the profile
with given name. Profiles may contain various sets of general options or
per-language options, including toolchain specifications.

Profiles are contained in the variable `agv::p::profiles`. Currently available
are only builtin profiles, currently those are available:

1. Compiler toolchain

* gcc-native : uses the gcc compiler
* clang-native : uses the clang compiler
* cc-custom : uses the compiler specified in the environment variables:
   * `CC` (the C compiler)
   * `CXX` (the C++ compiler)
   * `C/CXX/CPP/LD` + `FLAGS` : the compiler flags

2. Installation

* posix-install : applies installation rules according to GNU manners on POSIX systems

## `ag-config`

Usage:
```
ag-config avail <key> [-check] [-add] <values...> [-default] <values...> [-h helptext]
ag-config enable <key> <default> [-h helptext]
ag-config use <key> <values...>
ag-config have <key> <values...>
ag-config <category> <key> ?
```

Defines or completes a configuration item, available for the user to override.
The helptext is assigned to this option and will be presented when agcat is
called with `--options` argument.

Details depend on the category type:

* `avail`

This defines a set of options, single or multiple choice. If the options
are plain words, this is a single choice and you can select one of them
to be default. In this case default is the first specified option, unless
overridden by `-default` option. A value specified after `-default` is
simply added to the list if not specified earlier. In this case also
only the first option after `-default` is the default choice.

For specifying values of multiple choice, use `-check` option or prepend
every value by `*`. In this case every option after `-default` belongs
to the default set, and if default is not specified, it's empty.

There's no corresponding command line option - use `--use-*` for it.

* `use`

If used as a category for `ag-config`, it specifies the default choice
for particular option earlier defined by `ag-config avail` with the same
key (overrides any defaults specified by this). Corresponding command
line option `--use-*` overrides the default value.

* `enable`

For this category you have to specify the default value as a Tcl boolean
value (yes/no, true/false, on/off, 1/0).

The corresponding command line option is `--enable-<key>` with possible
boolean value or `--disable-<key>` which is specifying `false` for this
option.

* `have`

This category is reserved for system autodetection. It is detector type
dependent what it contains under the specified key.


Calling `[ag-config <category> <key>]` returns the current assigned value
of the option (default or user-overridden).

If the only argument following the key is `?`, it's testing the option's
existence - returns true if this option is defined.


## `ag-option`

Usage:
```
ag-option --<option> <default> [-f filter] [-a available] [-h helptext]
```

This is a free-form option that the build file can define. The `<option>`
name is of the user's choice, except that it is not allowed to begin
with any config-reserved keywords: enable, disable, avail, use, have.

The default value is obligatory and it contains simply a string defining
the option with the interpretation of the user's choice. The option can
be automatically verified using the following options:

* `-f <filter>`: the option must match a filter, possible values are
   * `~<mask>` : a glob-type pattern
   * `/<mask>` : a regexp-type pattern
   * `number` : a numeric value
   * `bool` : a Tcl-type boolean value 
   * `keyword` : characters allowed for keywords (`[a-zA-Z_][a-zA-Z0-9_]`)
* `-a <available...>`: a list of words that are the only valid options

## `ag-require`

Usage:
```
ag-require [-optional] <filename>
```

It's more-less the same as `source`, except that it also searches the
exact filename of the script in the following default locations:

* The first parent directory towards the one containing the agcat
script (usually additionally in `bin` directory), added with `lib`
and `share`

* The user's location overridden by `AG_LIB_PATH` environment variable.

If the first argument is `-optional`, the missing file is ignored,
otherwise it is an error if the file is not found.


## `ag-instantiate`

Usage:
```
ag-instantiate [options] <template-file> <target-file>
```

This creates a target file basing on the `<template-file>` by filling
requirements using the variables defined in this build file. Currently
no `[options]` are supported (this is left for future) and the only
template type file supported is the classic "in" file, where entries
with variables to be replaced are marked as `@<variable>@`. More types
can be supported in the future.

The `<target-file>` is a path of the instantiated file to be written;
if relateive, it's in the build directory.


## `ag-maf`

Usage:
```
ag-maf <target> <base-directory> <MAF-file> <specs...>
```

This command manages the data provided by a MAF file. It reads and
interprets the MAF file and then configures the `<target>` using the
data extracted by the entry specification. The data are usually
filenames, which's path is relative to the `<base-directory>`.

* `<target>`: the AG target to be configured
* `<base-directory>`: the directory base for MAF file and contained filenames
* `<MAF-file>`: the path to the MAF file relative to `<base-directory>`
* `<specs...>`: specifies `-key SECTION`, where
   * `-key`: the key in `<target>` to which the value should be applied
   * `SECTION`: the entry from which the value should be extracted

The MAF syntax is the following:

1. `#` is a comment

2. The first line should start with uppercase letter or a space, and this
declares the section name; it must follow an empty line

3. Everything that follows directly the section name is the part of the
section contents, up to the empty line

4. The section name may contain spaces, and only the `-` (dash) character
is special; if present, it specifies boolean flags that make the section
enabled.

If a section contains a conditional, the conditional flags after `-` are
to be searched among the currently set variables; they are expected to
contain boolean values, and the specification `!<flag>` makes an inversion.

Section names may be repeated and contents are glued together. If a section
with conditionals is not enabled according to the specified flags, its
contents is empty.

## `ag-info`

Specifies explicitly some properties of a file that are required for proper
generation of the building rules. The syntax is similar to `ag` except that
the first argument is the filename as a path relative to `$agv::srcdir`.
Supported keys:

* `-language`: Specifies the language of the source file
* `-includes`: Specifies the file on which the target object file depends


## `ag-declare-generated`

Speicifies the filename that is considered generated, that is, not being
the source file of the project. Generated files are then added to the
definition of the `reconfigure-ifneeded` target as output, which means that
they will be deleted when this target is considered, or the target needs
rebuilding if any of them were modified.


## `ag-subdir`

Defines subdirectories, which are considered integral part of this build
file. The directory should contain a separate build file on its own and
it will be processed in a slave interpreter.

## `ag-interp`

Usage:

```
ag-interp <target> <script>
```

Executes the Tcl command in the interpreter assigned to the `<target>`.
The `<target>` must be the subdirectory that has been previously declared
through `ag-subdir`. Special values of `<target>` are:

* `.` (dot): resolves to `eval <script>` call
* `..` (two dots): evaluates the script on the direct parent directory


## `ag-export`


