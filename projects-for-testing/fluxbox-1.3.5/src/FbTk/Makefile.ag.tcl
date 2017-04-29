#

source ../../config.ag

# CPPFLAGS?

ag-profile general -depspec cached

proc target args {
	ag FbTk {*}$args
}

target -type library

# We'll try to do using the ellimination method

target -s [pfind *.cc]


if { ![phas XFT] } {
	target -s {- XftFontImp.cc}
}

if { ![phas MULTIBYTE] } {
	target -s {- XmbFontImp.cc}
}

if { ![phas XPM] } {
	target -s {- ImageXPM.cc}
}

if { ![phas IMLIB2] } {
	target -s {- ImageImlib2.cc}
}

