" A more-less trial version of syntax highlight for vim
" Usable for both Tclmake and Silvercat files.

source $VIMRUNTIME/syntax/tcl.vim

let &iskeyword .= ",-"

syn region tcltkCommand matchgroup=tcltkWidgetColor start="\<rule\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclNumber,tclVarRef,tclString,tcltkCommand
syn region tcltkCommand matchgroup=tcltkWidgetColor start="\<phony\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclNumber,tclVarRef,tclString,tcltkCommand
syn region tcltkCommand matchgroup=tcltkWidgetColor start="\<dep-rule\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclNumber,tclVarRef,tclString,tcltkCommand

syn region tcltkCommand matchgroup=tcltkWidgetColor start="\<ag\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,silvercatSwitch,tclNumber,tclVarRef,tclString,tcltkCommand,tclEmbeddedStatement
syn region tcltkCommand matchgroup=tcltkWidgetColor start="\<ag-profile\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,silvercatSwitch,tclNumber,tclVarRef,tclString,tcltkCommand,tclEmbeddedStatement
syn region tcltkCommand matchgroup=tcltkWidgetColor start="\<ag-instantiate\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,silvercatSwitch,tclNumber,tclVarRef,tclString,tcltkCommand,tclEmbeddedStatement

syn keyword silvercatSwitch contained -s -h -hidir -nh -cflags -ldflags -packages -incdir -defines -depends -type -install -libspec -features -D -I -L

syn keyword tcltkCommandColor pget phas pset puncomment

hi def link silvercatSwitch Special

