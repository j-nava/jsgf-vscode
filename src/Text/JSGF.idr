module Text.JSGF

import public Text.JSGF.Tree.Concrete
import public Text.JSGF.Tree.Abstract
import public Text.JSGF.Parser
import public Text.Parser
import public Text.JSGF.Parser.Token

export
jsgfDocToTree : Doc -> Tree

export
jsgfParse : String -> ParseResult
jsgfParse = parseDoc
