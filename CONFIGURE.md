
CONFIGURE
=========

0. DISCLAIMER
-------------

This document describes a concept. Nothing that it describes is currently implemented,
nor is this document a complete descrption of anything.


1. Config database
------------------

This has to be a system, which performs particular general system autodetection
and allows a user to select options from alternatives.

The `agcat` tool when using options starting from `--`, uses one of the standard
configuration options. They all consist of functional prefix and the name. The
following functional prefix are used:

* --have - rather not used manually, this is for automated check, although this option
  can be used to override a system check. There's also inverted prefix: --havent
* --use - declares that particular thing should be used. These options may have
  parameters. This also has alternative `--use-no` in order to override a default,
  although particular options should also accept `--use-OPTION=none`
* --enable - boolean only values, set the value to true (alternative `--disable`
  prefix sets it to false).
* --with - specifies some detail of already accepted alternative, such as path
  or alternative specification. The `with` prefix should be used for something
  that is already enabled by `--enable` or used by `--use`, or when it's turned
  on by default. Anything specified by --with must have some existing default value.
  Default is enforced with --without specification or --with-OPTION=default.

All these options later fill details in the ag-config database. Access is gained
through the following command, for example:

    ag-config have cxx11

All `have` and `enable` entries return a boolean value. The `use` and `with`
entries return a text - by default it's `1` (an empty text means an unset option).

An autodetection process usually sets automatically entries in `have` and `with`
database. Entries in `use` and `enable` are set by a user. A user may also set
options in `have` and `with` databases to override or add system settings.


2. Builtin configuration features
---------------------------------

* --with-buildtype:
   * release (default)
   * debug
   * debug-opt
* --with-profile: enforce Silvercat to load given profile (multiple profiles can be passed)
* --with-agpath: add paths to Silvercat shared files (profiles and frameworks used by ag-load)
* --have-pkg: check if pkg-config provides package with given name


3. Typical scenarios for configure
----------------------------------

### A. Autodetection with enabler

There are a couple of standard tests (particular modules may enhance these test abilities).
The most important test - and the most generic one - is pkg-config. There's also a standard
feature test, --have-pkg.

You put in the config sheet the following statement:

    ag-check-have pkg <package-name> ?version?

This will cause the following databases to be updated:

* `have` will be added a key named 'pkg:<package>'.
* `with` will be added a key named 'includes:<package>' that extracts the include path and flags
* `with` will be added a key named 'libs:<package>' that extracts the library path and flags

Note that the package can be mandatory or optional, as well as the `enable` flag for that package
may exist and be set to false, in which case the check isn't being done. So, all the above updates
are NOT being done in case when `ag-config enable pkg:<package>` returns false.

Note that:

* If there already exists the `enable` key for that package, additional informational text is
displayed informing the user that they can disable the package and this way continue without it.
* If there's no `enable` key for that package, the package is considered obligatory and there's
no way to go without it.

Set the `enabled` to true before the check in order to make the information available to the user.
The user may still want to continue without the package, although in this case they do it on their
own responsibility.

This check by default causes exception-interrupt (or continue checks with error, if -k) if the
package isn't found. You can override this behavior by using optional flags.

If the situation is that the package is found and therefore appropriate with includes/libs keys
have been filled in, these things can be still overridden (i.e. keys are set before the check
is done, which means that it won't be newly set by ag-check-have) by using --with option. This
has then the following syntax:

    --with-<pkgname>-includes=PATH --with-<pkgname>-libs=PATH

or, if the standard-layout directories are used (lib and include), you can simplify it to:

    -with-<pkgname>-prefix=PATH
	# uses PATH/lib for libs and PATH/include for includes

Note that you can also create an alias package name, which can translate the used package name
into a valid package name available in pkg-config database. Searching in alias database has higher
priority than actual package name. You can create alias by specifying

    ag-pkg-config-alias <alias name> <package name>

Particular other check tests can also define different ways to extract specification. Not every
check should update any keys in `with` database. The keys `with includes:<name>` and `with libs:<name>`
must be present at least as empty if they are meant to be overridable by user.

The help text that is displayed by user's request reads also the whole config sheet and uses
it to show the user-configurable items.


### B. Use the --use option to specify an alternative

The --use option 

