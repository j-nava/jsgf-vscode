module Text.JSGF.Lexer

import Text.Lexer
import Text.JSGF.Token

text : Lexer
text = someUntil (oneOf "[]<>{}().#;" <|> spaces) any -- pred (> chr 160) <|> alphaNum

tokenMap : TokenMap JSGFToken
tokenMap = toTokenMap
  [ (spaces,  JSGFSpace)
  , (is '[',  JSGFLBracket)
  , (is ']',  JSGFRBracket)
  , (is '<',  JSGFLAngBracket)
  , (is '>',  JSGFRAngBracket)
  , (is '{',  JSGFLCurlyBracket)
  , (is '}',  JSGFRCurlyBracket)
  , (is '(',  JSGFLParens)
  , (is ')',  JSGFRParens)
  , (is '.',  JSGFDot)
  , (is '#',  JSGFDash)
  , (is ';',  JSGFSemi)
  , (text,    JSGFText)
  ]

export
jsgfLex : String -> List (WithBounds JSGFToken)
jsgfLex s = 
  let (tokens, _) = lex tokenMap s 
  in tokens
