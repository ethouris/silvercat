" A more-less trial version of syntax highlight for vim
" Usable for both Tclmake and Silvercat files.

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  if b:current_syntax == "tclmake"
     " We're done here
     finish
  elseif b:current_syntax == "tcl"
	" We've passed Tcl syntax, but not yet this file. Continue
  else
	" We have something else, so clear syntax and apply Tcl syntax first
	syntax clear
	unlet b:current_syntax
	source $VIMRUNTIME/syntax/tcl.vim
  endif
else
	source $VIMRUNTIME/syntax/tcl.vim
endif


if index(split(&iskeyword, ","), "-") == -1 
	setlocal iskeyword+=-
endif

command! -nargs=1 TclmakeDefineCommand :syn region tcltkCommand matchgroup=tcltkWidgetColor start="\<<args>\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,silvercatSwitch,tclNumber,tclVarRef,tclString,tcltkCommand,tclEmbeddedStatement

" make.tcl

TclmakeDefineCommand rule
TclmakeDefineCommand phony
TclmakeDefineCommand dep-rule
TclmakeDefineCommand setflags
TclmakeDefineCommand getflags

syn keyword tcltkCommandColor pget phas pset pset+ pinit puncomment pexpand pdef pdefx pdefv
syn keyword tcltkCommandColor pwrite pupdate pread pmap pfind prelativize plist 


" ag.tcl

TclmakeDefineCommand ag
TclmakeDefineCommand ag-profile
TclmakeDefineCommand ag-instantiate
TclmakeDefineCommand ag-subdir
TclmakeDefineCommand ag-make
TclmakeDefineCommand ag-export


syn keyword silvercatSwitch contained -s -sources -h -headers -nh -noinst-headers -hidir -cflags -L -ldflags -lflags -libs
\ -packages -I -incdir -D -defines -depends -type -install -libspec -features -o -output -command -clean
\ -depspec -imgen 

hi def link silvercatSwitch Special

