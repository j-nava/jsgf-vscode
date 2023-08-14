module Text.JSGF

import Data.String
import Text.Parser
import Text.JSGF.Token
import public Text.JSGF.Types
import Text.JSGF.Lexer
import Text.JSGF.Parser

export
jsgfParse : String -> Either String MDDoc
jsgfParse = buildResult . mdParse . mdLex

  where
  buildResult : Either (List1 (ParsingError MDToken)) MDDoc -> Either String MDDoc
  buildResult = \case
    Right d => Right d
    Left err => Left $ show err

export
jsgfEmpty : MDDoc
jsgfEmpty = Paragraph ((Textual "") ::: Nil) ::: Nil
