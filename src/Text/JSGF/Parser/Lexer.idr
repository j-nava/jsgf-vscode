module Text.JSGF.Parser.Lexer

import Text.Lexer
import Text.JSGF.Parser.Token

text : Lexer
text = someUntil (oneOf "[]<>{}().#;*" <|> spaces) any -- pred (> chr 160) <|> alphaNum

comment : Lexer
comment = is '/' <+> is '/' <+> many (isNot '\n')

commentBlock : Lexer
commentBlock = is '/' <+> is '*' <+> manyThen (is '*' <+> is '/') any

tokenMap : TokenMap JSGFToken
tokenMap = toTokenMap
  [ (spaces,         JSGFSpace)
  , (comment,        JSGFSpace)
  , (commentBlock,   JSGFSpace)
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
lex : String -> List (WithBounds JSGFToken)
lex s = 
  let (tokens, _) = lex tokenMap s 
  in tokens
