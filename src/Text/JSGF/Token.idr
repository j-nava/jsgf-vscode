module Text.JSGF.Token

import Data.String
import Text.Lexer
import Language.Reflection.Util
import Derive.Eq
import Derive.Show

%language ElabReflection

public export
data JSGFTokenKind
  = JSGFSignature
  | JSGFSpace
  | JSGFDot
  | JSGFSemi
  | JSGFLBracket
  | JSGFRBracket
  | JSGFLAngBracket
  | JSGFRAngBracket
  | JSGFLCurlyBracket
  | JSGFRCurlyBracket
  | JSGFLParens
  | JSGFRParens
  | JSGFText

%runElab derive "JSGFTokenKind" [Eq,Show]

public export
TokenKind JSGFTokenKind where
  TokType JSGFSignature = ()
  TokType JSGFSpace = String
  TokType JSGFDot = String
  TokType JSGFSemi = ()
  TokType JSGFLBracket = ()
  TokType JSGFRBracket = ()
  TokType JSGFLAngBracket = ()
  TokType JSGFRAngBracket = ()
  TokType JSGFLCurlyBracket = ()
  TokType JSGFRCurlyBracket = ()
  TokType JSGFLParens = ()
  TokType JSGFRParens = ()
  TokType JSGFText = String

  tokValue JSGFSignature _ = ()
  tokValue JSGFSpace s = s
  tokValue JSGFDot _ = "."
  tokValue JSGFSemi _ = ()
  tokValue JSGFLBracket _ = ()
  tokValue JSGFRBracket _ = ()
  tokValue JSGFLAngBracket _ = () 
  tokValue JSGFRAngBracket _ = ()
  tokValue JSGFLCurlyBracket _ = ()
  tokValue JSGFRCurlyBracket _ = ()
  tokValue JSGFLParens _ = ()
  tokValue JSGFRParens _ = ()
  tokValue JSGFText s = s

public export
JSGFToken : Type
JSGFToken = Token JSGFTokenKind

public export
Show JSGFToken where
  show (Tok _ t) = t
