" Vim syntax file
" Language:    AADL
" Maintainer:  Thomas Vergnaud <thomas.vergnaud@aist.enst.fr>
" Last Change: 2007-03-05

if version < 600
	syntax clear
elseif exists ("b:current_syntax")
	finish
endif

" AADL is case-insensitive
syntax case ignore

syntax keyword aadlKeyword is features subcomponents extends modes properties connections flows annexes calls applies to in out event initial mode flow sink source annex inherit constant requires provides server

syntax match aadlEntity /\(bus\|virtual\s\+processor\|virtual\s\+bus\|data\s\+access\|data\|device\|memory\|process\|processor\|subprogram\|system\|thread\s\+group\|thread\|port\s\+group\|end\|property\s\+set\|package\|parameter\)\(\s\+implementation\)\=\>/ nextgroup=aadlDefinition skipwhite 
syntax match aadlDefinition /\(\w\+::\)*\w\+\(\.\w\+\)\=/ contained

syntax keyword aadlType aadlstring aadlboolean aadlinteger aadlfloat 
syntax keyword aadlStruct enum reference classifier
syntax keyword aadlOperator and or not

syntax region aadlString start=/"/ skip=/\\"/ end=/"/

syntax region aadlBlock matchgroup=aadlBrace start=/{/ end=/}/ transparent fold
syntax region aadlAnnex matchgroup=aadlAnnexmark start=/{\*\*/ end=/\*\*}/ transparent fold

syntax keyword aadlTodo TODO XXX FixMe contained
syntax match aadlComment /--.*/ contains=aadlTodo

if version >= 508 || !exists("did_aadl_syn_inits")
  if version < 508
    let did_aadl_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink aadlKeyword 		Keyword
  HiLink aadlEntity 		Keyword
  HiLink aadlComment		Comment
  HiLink aadlTodo		Todo
  HiLink aadlString		String
  HiLink aadlOperator		Operator
  HiLink aadlStruct		Structure
  HiLink aadlType		Type
  HiLink aadlDefinition		Type
  HiLink aadlBrace		Delimiter
  HiLink aadlAnnexmark		Delimiter

  delcommand HiLink
endif

let b:current_syntax = "aadl"


