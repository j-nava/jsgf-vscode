module Text.JSGF.Token

import Data.String
import Text.Lexer
import Language.Reflection.Util
import Derive.Eq
import Derive.Show

%language ElabReflection

public export
data JSGFTokenKind
  = JSGFText
  | JSGFSpace
  | JSGFDot
  | JSGFLineBreak
  | JSGFTab
  | JSGFLBracket
  | JSGFRBracket
  | JSGFLAngBracket
  | JSGFRAngBracket
  | JSGFLCurlyBracket
  | JSGFRCurlyBracket
  | JSGFLParens
  | JSGFRParens

  -- | MDBacktick
  -- | MDAsterisk
  -- | MDUnderscore
  -- | MDHash
  -- | MDDash

%runElab derive "JSGFTokenKind" [Eq,Show]

public export
TokenKind JSGFTokenKind where
  TokType JSGFText = String
  TokType JSGFSpace = String
  TokType JSGFDot = String
  TokType JSGFLineBreak = String
  TokType JSGFTab = String
  TokType JSGFLBracket = String
  TokType JSGFRBracket = String
  TokType JSGFLAngBracket = String
  TokType JSGFRAngBracket = String
  TokType JSGFLCurlyBracket = String
  TokType JSGFRCurlyBracket = String
  TokType JSGFLParens = String
  TokType JSGFRParens = String

  tokValue JSGFText s = s
  tokValue JSGFSpace _ = " "
  tokValue JSGFDot _ = "."
  tokValue JSGFLineBreak s = s
  tokValue JSGFTab _ = "\t"
  tokValue JSGFLBracket _ = "]"
  tokValue JSGFRBracket _ = "]"
  tokValue JSGFLAngBracket _ = "<"
  tokValue JSGFRAngBracket _ = ">"
  tokValue JSGFLCurlyBracket _ = "{"
  tokValue JSGFRCurlyBracket _ = "}"
  tokValue JSGFLParens _ = "("
  tokValue JSGFRParens _ = ")"

public export
JSGFToken : Type
JSGFToken = Token JSGFTokenKind

public export
Show JSGFToken where
  show (Tok _ t) = t
