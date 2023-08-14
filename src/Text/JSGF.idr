module Text.JSGF

import Data.String
import Text.Parser
import Text.JSGF.Token
import public Text.JSGF.Types
import Text.JSGF.Lexer
import Text.JSGF.Parser

export
jsgfParse : String -> Either String Doc
jsgfParse = buildResult . jsgfParse . jsgfLex

  where
  buildResult : Either (List1 (ParsingError JSGFToken)) Doc -> Either String Doc
  buildResult = \case
    Right d => Right d
    Left err => Left $ show err

export
jsgfEmpty : Doc
jsgfEmpty = BSpace ::: Nil
