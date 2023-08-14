module Text.JSGF.Lexer

import Text.Lexer
import Text.JSGF.Token

tokenMap : TokenMap JSGFToken
tokenMap = toTokenMap
  [ (is ' ',  JSGFSpace)
  , (is '[',  JSGFLBracket)
  , (is ']',  JSGFRBracket)
  , (is '<',  JSGFLAngBracket)
  , (is '>',  JSGFRAngBracket)
  , (is '{',  JSGFLCurlyBracket)
  , (is '}',  JSGFRCurlyBracket)
  , (is '(',  JSGFLParens)
  , (is ')',  JSGFRParens)
  , (is '\t', JSGFTab)
  , (is '.',  JSGFDot)
  , (newline, JSGFLineBreak)
  , (any,     JSGFText)
  ]

export
jsgfLex : String -> List (WithBounds JSGFToken)
jsgfLex s = 
  let (tokens, _) = lex tokenMap s 
  in tokens
