" Vim indent file 
" Language:    AADL
" Maintainer:  Thomas Vergnaud <thomas.vergnaud@aist.enst.fr>
" Last Change: 2007-03-05

if exists ("b:did_indent")
   finish
endif
let b:did_indent = 1

setlocal indentexpr=GetAadlIndent()
setlocal indentkeys-=0{,0}
setlocal indentkeys+=0=~public,0=~private,0=~property,0=~features,0=~flows,0=~modes,0=~subcomponents,0=~calls,0=~properties,0=~connections,0=~annexes

" Only define the functions once.
if exists ("*GetAadlIndent")
   finish
endif

let s:aadlDeclarationStart = '^\s*\(data\>\|device\>\|memory\>\|system\>\|process\>\|processor\>\|subprogram\>\|thread group\>\|thread\>\|bus\>\|port group\>\)'
let s:aadlSubclauseStart = '^\s*\(features\>\|subcomponents\>\|modes\>\|calls\>\|flows\>\|annexes\>\|properties\>\|connections\>\)'
let s:aadlNamespaceStart = '^\s*\(property set\>\|package\>\|public\>\|private\>\)'
let s:aadlComment = "\\v^(\"[^\"]*\"|'.'|[^\"']){-}\\zs\\s*--.*"

" Find the indentation value of block and return it. Return default_indent if
" block is not found. Research is performed backward from line number
" current_lnum
function s:aadlFindIndent (current_lnum, block, default_indent)
   let lnum = a:current_lnum - 1

   while lnum >= 0
      let line = getline (lnum)

      if line =~ a:block
	 return indent (lnum)
      endif

      let lnum = lnum - 1
   endwhile

   return a:default_indent
endfunction

" Return 1 if the line at ref_lnum seems to contain an AADL declaration;
" else return 0
function s:isDeclaration(prev_lnum)
   let lnum  = a:prev_lnum
   while lnum > 0
      let prev_lnum = lnum
      let lnum = prevnonblank (lnum - 1)
      " Get previous non-blank/non-comment-only line
      while 1
	 let line = substitute (getline(lnum), s:aadlComment, '', '')
	 if line !~ '^\s*$'
	    break
	 endif
	 let lnum = prevnonblank (lnum - 1)
	 if lnum <= 0
	    " if we did not find any relevant statement, then we are dealing
	    " with a declaration
	    return 1
	 endif
      endwhile
      " Leave indent alone if our ';' line is part of a ';'-delineated
      " aggregate (e.g., procedure args.) or first line after a block start.
      if line =~ '^\s*end\>'
	 " if we find a statement 'end', then we are dealing with a
	 " declaration
	 return 1
      elseif line =~ s:aadlDeclarationStart
	 " if we find a declaration, then we are dealing with a subclause
	 " statement; probably a connection
	 return 0
      endif
   endwhile
endfunction

function s:getParentIndent(prev_lnum)
  let parenDepth = 0
  let lnum = a:prev_lnum
  while lnum > 0
    let prev_lnum = lnum
    let lnum = prevnonblank (lnum - 1)
    " Get previous non-blank/non-comment-only line
    while 1
      let line = substitute (getline(lnum), s:aadlComment, '', '')
      if line !~ '^\s*$'
	break
      endif
      let lnum = prevnonblank (lnum - 1)
      if lnum <= 0
	" if we did not find any relevant statement, then we are dealing
	" with a declaration
	return 1
      endif
    endwhile

    let line = getline (lnum)
    if line =~ '{'
      let parenDepth = parenDepth + 1
    endif
    if line =~ '}'
      let parenDepth = parenDepth - 1
    endif

    if parenDepth > 0
      if line =~ '^\s*{'
	return indent (lnum) - &sw + 1
      else
	return indent (lnum) 
      endif
    elseif line =~ s:aadlNamespaceStart
      return indent (lnum)
    elseif line =~ s:aadlDeclarationStart && s:isDeclaration (line)
      return indent (lnum)
    elseif line =~ s:aadlSubclauseStart
      return indent (lnum)
    endif
  endwhile
  return 0
endfunction

" Main indentation function
function GetAadlIndent()
   " Check current line; search for simplistic matching start-of-block
   let line = getline (v:lnum)
   let ind = 0;
   " Check for potential argument list on next line
   let continuation = (line =~ '[A-Za-z0-9_]\s*$')

   if line =~ s:aadlNamespaceStart
      let ind = 0

   elseif line =~ s:aadlSubclauseStart 
      let ind = s:aadlFindIndent (v:lnum, s:aadlDeclarationStart, 0)

   elseif line =~ s:aadlDeclarationStart && s:isDeclaration (v:lnum)
      let ind = s:aadlFindIndent (v:lnum, s:aadlNamespaceStart, -&sw) + &sw

   elseif line =~ '^\s*end\>'
      let lnum = v:lnum
      let ind = 0
      while lnum > -1
	 let line = getline (lnum)
	 if line =~ s:aadlDeclarationStart 
	    || line =~ s:aadlNamespaceStart
	    let ind = indent (lnum)
	    break
	 endif
	 let lnum = lnum - 1 
      endwhile

   elseif continuation && line =~ '^\s*('
      let ind = ind + &sw

   else	
      " Find a non-blank line above the current line.
      let lnum = prevnonblank (v:lnum - 1)
      let ind = indent (lnum)

      " Get previous non-blank/non-comment-only
      while 1
	 let line = substitute (getline (lnum), s:aadlComment, '', '')
	 if line !~ '^\s*$'
	    break
	 endif
	 let lnum = prevnonblank (lnum - 1)
	 if lnum <= 0
	    return 0
	 endif
      endwhile

      " Get default indent (from parent block)
      let ind = s:getParentIndent (v:lnum) + &sw
    endif
  endif

  return ind
endfunction

