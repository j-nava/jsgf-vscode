module Text.JSGF

import public Text.Parser

import Data.String
import public Text.JSGF.Token
import public Text.JSGF.Types
import Text.JSGF.Lexer
import Text.JSGF.Parser

public export
ParseResult : Type
ParseResult = Either (List1 (ParsingError JSGFToken)) Doc

export
jsgfParseDoc : String -> ParseResult
jsgfParseDoc = jsgfParse . jsgfLex
