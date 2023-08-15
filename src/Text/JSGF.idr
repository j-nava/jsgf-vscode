module Text.JSGF

import Text.Parser

import Data.String
import Text.JSGF.Token
import public Text.JSGF.Types
import Text.JSGF.Lexer
import Text.JSGF.Parser

public export
JSGFParsingError : Type
JSGFParsingError = List1 (ParsingError JSGFToken)

public export
ParseResult : Type
ParseResult = Either (List JSGFParsingError) Doc

export
jsgfParse : String -> ParseResult
jsgfParse = buildResult . jsgfParse . jsgfLex

  where
  buildResult : Either JSGFParsingError Doc -> ParseResult
  buildResult = \case
    Right d => Right d
    Left err => Left $ []

export
jsgfEmpty : Doc
jsgfEmpty = BSpace ::: Nil
