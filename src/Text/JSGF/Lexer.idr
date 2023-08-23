module Text.JSGF.Lexer

import Text.Lexer
import Text.JSGF.Token

text : Lexer
text = someUntil (oneOf "[]<>{}().#;" <|> spaces) any -- pred (> chr 160) <|> alphaNum

tokenMap : TokenMap JSGFToken
tokenMap = toTokenMap
  [ (spaces,         JSGFSpace)
  , (exact "#JSGF",  JSGFSignature)
  , (oneOf "([{<",   JSGFOpen)
  , (oneOf ")]}>",   JSGFClose)
  , (is '.',         JSGFDot)
  , (is ';',         JSGFSemi)
  , (is '=',         JSGFEquals)
  , (is '|',         JSGFPipe)
  , (is '*',         JSGFStar)
  , (is '+',         JSGFPlus)
  , (text,           JSGFText)
  ]

export
jsgfLex : String -> List (WithBounds JSGFToken)
jsgfLex s = 
  let (tokens, _) = lex tokenMap s 
  in tokens
