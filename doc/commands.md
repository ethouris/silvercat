
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

Profilespec can be:

* `general` - sets the option applicable for every target (if propagated)
* A single language name or a list of languages in { ... } (e.g. `{c c++}`)

Options get applicable to every target for which the effective language is
the one specified in the list, or to all targets if `general`.

If the call is with only one argument, `<profilename>`, it loads the profile
with given name. Profiles may contain various sets of general options or
per-language options, including toolchain specifications.

# `ag-config`

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
overridden by `-default` option. An value specified after `-default` is
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
value.

The corresponding command line option is `--enable-<key>` with possible
boolean value or `--disable-<key>` which is specifying `false` for this
option.

* `have`

This category is reserved for system autodetection. It is detector type
defined what it contains under the specified key.

Calling `[ag-config <category> <key>]` returns the current assigned value
of the option (default or user-overridden).

If the only argument following the key is `?`, this is an expression
returns the boolean value of whether the option is defined.


# `ag-option`

Usage:
```
ag-option --<option> <default> [-f filter] [-a available] [-h helptext]
```

This is a free-form option that the build file can define. Note that if
the option name consists of `<headword>-...` then it is not allowed that
`<headword>` be any of the configuration keys: enable, disable, avail,
use, have.

The default value is obligatory and it contains simply a string defining
the option with the interpretation of the user's choice. The option can
be automatically verified using the following options:

* `-f <filter>`: the option must match a filter, possible values are
   * `~<mask>` : a glob-type pattern
   * `/<mask>` : a regexp-type pattern
   * number : a numeric value
   * bool : a Tcl-type boolean value 
   * keyword : letters, numbers and underscore
* `-a available`: a list of words that are the only valid options

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

If the first argument is `-optional`, the missing file is ignored.

## `ag-instantiate`
## `ag-maf`
## `ag-info`
## `ag-declare-generated`
## `ag-subdir`
## `ag-export`


