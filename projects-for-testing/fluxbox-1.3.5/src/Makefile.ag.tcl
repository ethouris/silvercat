
source $agv::toplevel/config.ag

set PROGRAM_PREFIX [pget PROGRAM_PREFIX NONE]
set PROGRAM_SUFFIX [pget PROGRAM_SUFFIX NONE]

ag-export {
	PROGRAM_PREFIX
	PROGRAM_SUFFIX
}

ag-profile c++ -incdir $agv::srcdir/FbTk
ag-profile general -depspec cached

set defaults_tpl_h {
// This file is generated from Makefile. Do not edit!
#include <string>

#define DEFAULTMENU \"$DEFAULT_MENU\"
#define DEFAULTSTYLE \"$DEFAULT_STYLE\"
#define DEFAULTKEYSFILE \"$DEFAULT_KEYS\"
#define DEFAULT_APPSFILE \"$DEFAULT_APPSFILE\"
#define DEFAULT_OVERLAY \"$DEFAULT_OVERLAY\"
#define DEFAULT_INITFILE \"$DEFAULT_INIT\"
#define DEFAULT_WINDOWMENU \"$DEFAULT_WINDOWMENU\"
#define PROGRAM_PREFIX \"$PROGRAM_PREFIX\"
#define PROGRAM_SUFFIX \"$PROGRAM_SUFFIX\"
std::string realProgramName(std::string name);
const char* gitrevision(void);
}

set defaults_tpl_c {
#include "defaults.hh"

std::string realProgramName(std::string name) {
  return PROGRAM_PREFIX + name + PROGRAM_SUFFIX;
}

const char* gitrevision(void) {
  return "$sver";
}

}

proc generate-defaults-file {indir} {

	puts "Generating defaults.cc and defaults.hh files"

	set hfile [pexpand $::defaults_tpl_h]
	set gitdir [file join $::TOP .git]
	if { [file isdirectory $gitdir] && [file readable $gitdir/HEAD] } {
		set head [pread $gitdir/HEAD]
		set headref [string trim [lindex [split $head " "] 1]]
		set sver [pread $gitdir/$headref]
	} else {
		set sver this_is_tar_ball_build
	}

	set cfile [pexpand $::defaults_tpl_c 1] ;# we need 'sver' from HERE, not global

	pupdate $indir/defaults.hh $hfile
	pupdate $indir/defaults.cc $cfile
}

ag-export {
	generate-defaults-file
	defaults_tpl_c
	defaults_tpl_h
}

ag generated-defaults -type custom -output //defaults.hh //defaults.cc -s $agv::toplevel/config.ag -command {
	!tcl generate-defaults-file [prelocate $agv::srcdir]
}


ag-subdir FbTk

ag fluxbox {
	-type program
	-install bin
	-depends FbTk/FbTk
	-ldflags -- -liconv
}

# XXX theoretically this should be achieved by 'ag fluxbox -depends generated-defaults'.
# However, adding file-dependencies by -depends for dependent target of type custom
# is not yet implemented. There could be made something like file type autodetection
# for custom targets (only those that are declared explicitly), and qualify automatically
# *.c* files as -source and *.h files as -noinst-header.
ag fluxbox -s defaults.cc -depends generated-defaults

puts "Going to generate defaults files in advance"

# Without cached dependencies, this file must be made "in place", otherwise dep commands will fail.
# Generate it also the first time
generate-defaults-file $agv::srcdir

# XXX This doesn't work - it states the 'generate-defaults-file' is undefined.
# Probably it works inside different environment.
#ag-make generated-defaults



# Common sources
ag fluxbox -s {
	ArrowButton.cc
	FbAtoms.cc
	FbWinFrame.cc
	FbWinFrameTheme.cc
	fluxbox.cc
	Keys.cc
	main.cc
	RootTheme.cc
	FbRootWindow.cc
	OSDWindow.cc
	TooltipWindow.cc
	Screen.cc
	WinButton.cc
	WinButtonTheme.cc
	Window.cc
	WindowState.cc
	Workspace.cc
	FbCommands.cc
	LayerMenu.cc
	FbMenu.cc
	WinClient.cc
	Xutil.cc
	CurrentWindowCmd.cc
	WorkspaceCmd.cc
	TextDialog.cc
	CommandDialog.cc
	SendToMenu.cc
	AlphaMenu.cc
	FbMenuParser.cc
	StyleMenuItem.cc
	RootCmdMenuItem.cc
	MenuCreator.cc
	ClientMenu.cc
	ClientPattern.cc
	WorkspaceMenu.cc
	HeadArea.cc
	Resources.cc
	WindowCmd.cc
	FocusControl.cc
	CascadePlacement.cc
	ColSmartPlacement.cc
	MinOverlapPlacement.cc
	RowSmartPlacement.cc
	ScreenPlacement.cc
	UnderMousePlacement.cc
	AttentionNoticeHandler.cc
	IconButton.cc
	IconbarTheme.cc
	FocusableList.cc
}

# Conditional adding sources
if { [phas NEWWMSPEC] } {
	ag fluxbox -s Ewmh.cc
}

if { [phas GNOME] } {
	ag fluxbox -s Gnome.cc
}

if { [phas REMEMBER_SRC] } {
	ag fluxbox -s Remember.cc RegExp.cc ClientPattern.cc
}

if { [phas TOOLBAR_SRC] } {
	ag fluxbox -s {
		Toolbar.cc
		ToolbarTheme.cc
		ToolbarItem.cc
		ClockTool.cc
		WorkspaceNameTool.cc
		IconbarTool.cc
		IconbarTheme.cc
		ToolTheme.cc
		IconButton.cc
		SystemTray.cc
		GenericTool.cc
		ButtonTool.cc
		ButtonTheme.cc
		ToolFactory.cc
	}
}


