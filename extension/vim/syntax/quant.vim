" Vim syntax file
" Language: Quant
" Maintainer: Sigmapitech
" Latest Revision: 18 January 2026

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword quantKeyword fn return if else for while continue break import from const
syn keyword quantType int float bool str void
syn keyword quantBoolean True False

" Comments
syn match quantLineComment "//.*$"
syn match quantHashComment "#.*$"
syn region quantBlockComment start="/\*" end="\*/"

" Strings
syn region quantString start='"' end='"' skip='\\"'
syn region quantString start="'" end="'" skip="\\'"

" Numbers
syn match quantNumber '\<0[xX][0-9a-fA-F]\+\>'
syn match quantNumber '\<0[oO][0-7]\+\>'
syn match quantNumber '\<0[bB][01]\+\>'
syn match quantNumber '\<\d\+\(\.\d\+\)\?\>'

" Operators
syn match quantOperator "[-+*/%<>=!&|^~]"
syn match quantOperator "<<\|>>\|<=\|>=\|==\|!=\|&&\|||\|+="
syn match quantOperator "-=\|\*=\|/=\|%=\|&=\||=\|\^=\|<<=\|>>="

" Functions
syn match quantFunction '\<[a-zA-Z_]\w*\>\s*\ze('

" Delimiters
syn match quantDelimiter "[{}()\[\];:,.]"

" Link to highlight groups
hi def link quantKeyword Keyword
hi def link quantType Type
hi def link quantBoolean Boolean
hi def link quantLineComment Comment
hi def link quantHashComment Comment
hi def link quantBlockComment Comment
hi def link quantString String
hi def link quantNumber Number
hi def link quantOperator Operator
hi def link quantFunction Function
hi def link quantDelimiter Delimiter

let b:current_syntax = "quant"
