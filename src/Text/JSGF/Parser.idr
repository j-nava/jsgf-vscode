module Text.JSGF.Parser

import public Text.Parser

import public Text.JSGF.Parser.Token
import Text.JSGF.Parser.Lexer
import Text.JSGF.Parser.Parser
import Text.JSGF.Tree.Concrete

public export
ParseResult : Type
ParseResult = Either (List1 (ParsingError JSGFToken)) Doc

export
parseDoc : String -> ParseResult
parseDoc = parse . lex
