module Text.JSGF.Parser

import public Text.Parser

import public Text.JSGF.Parser.Token
import Text.JSGF.Parser.Lexer
import Text.JSGF.Parser.Parser
import Text.JSGF.Tree.Concrete

public export
Result : Type -> Type
Result a = Either (List1 (ParsingError JSGFToken)) a

export
parseDoc : String -> Result Doc
parseDoc = parse . lex
