module Text.JSGF.Token

import Data.String
import Text.Lexer
import Language.Reflection.Util
import Derive.Eq
import Derive.Show

%language ElabReflection

public export
data JSGFBracketType
  = JSGFInvalidBracket
  | JSGFParens
  | JSGFSquareBracket
  | JSGFAngBracket
  | JSGFCurlyBracket

export
Show JSGFBracketType where
  show JSGFInvalidBracket = "invalid"
  show JSGFParens         = "()"
  show JSGFSquareBracket  = "[]"
  show JSGFAngBracket     = "<>"
  show JSGFCurlyBracket   = "{}"

%runElab derive "JSGFBracketType" [Eq]

public export
data JSGFTokenKind
  = JSGFSignature
  | JSGFSpace
  | JSGFDot
  | JSGFSemi
  | JSGFOpen
  | JSGFClose
  | JSGFEquals
  | JSGFPipe
  | JSGFStar
  | JSGFPlus
  | JSGFText

%runElab derive "JSGFTokenKind" [Eq,Show]

public export
TokenKind JSGFTokenKind where
  TokType JSGFSignature = ()
  TokType JSGFSpace = String
  TokType JSGFDot = String
  TokType JSGFSemi = ()
  TokType JSGFOpen = JSGFBracketType
  TokType JSGFClose = JSGFBracketType
  TokType JSGFEquals = ()
  TokType JSGFPipe = String
  TokType JSGFStar = String
  TokType JSGFPlus = String
  TokType JSGFText = String

  tokValue JSGFSignature _ = ()
  tokValue JSGFSpace s = s
  tokValue JSGFDot _ = "."
  tokValue JSGFSemi _ = ()
  tokValue JSGFOpen "(" = JSGFParens
  tokValue JSGFOpen "[" = JSGFSquareBracket
  tokValue JSGFOpen "{" = JSGFCurlyBracket
  tokValue JSGFOpen "<" = JSGFAngBracket
  tokValue JSGFOpen _ = JSGFInvalidBracket
  tokValue JSGFClose ")" = JSGFParens
  tokValue JSGFClose "]" = JSGFSquareBracket
  tokValue JSGFClose "}" = JSGFCurlyBracket
  tokValue JSGFClose ">" = JSGFAngBracket
  tokValue JSGFClose _ = JSGFInvalidBracket
  tokValue JSGFEquals _ = ()
  tokValue JSGFPipe s = s
  tokValue JSGFStar s = s
  tokValue JSGFPlus s = s
  tokValue JSGFText s = s

public export
JSGFToken : Type
JSGFToken = Token JSGFTokenKind

public export
Show JSGFToken where
  show (Tok _ t) = t
