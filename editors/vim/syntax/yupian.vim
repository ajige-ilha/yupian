if exists("b:current_syntax")
  finish
endif

syn case match
syn keyword yupianKeyword 木 水 火 土 竹 十 戈 大 中 一 弓 人 心 手 口 尸 廿 山 田 難 卜
syn match yupianNumber "[-+]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)"
syn region yupianString start='"' skip='\\"' end='"'
syn match yupianComment ";.*$"
syn match yupianDelimiter "[()「」[]『』]"
syn match yupianIdentifier "\<[[:alnum:]_?-][[:alnum:]_?!-]*\>"

hi def link yupianKeyword Keyword
hi def link yupianNumber Number
hi def link yupianString String
hi def link yupianComment Comment
hi def link yupianDelimiter Delimiter
hi def link yupianIdentifier Identifier

let b:current_syntax = "yupian"
