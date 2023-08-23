module Text.JSGF.Parser

import Data.String
import Text.Parser
import Text.JSGF.Token
import Text.JSGF.Types

matchtok : TokenKind JSGFTokenKind => Eq JSGFTokenKind => (kind : JSGFTokenKind) -> Grammar state (Token JSGFTokenKind) True (TokType kind, Maybe (TokType JSGFSpace))
matchtok tok = do
  v <- match tok
  pure (v, Nothing)

matchsp : TokenKind JSGFTokenKind => Eq JSGFTokenKind => (kind : JSGFTokenKind) -> Grammar state (Token JSGFTokenKind) True (TokType kind, Maybe (TokType JSGFSpace))
matchsp tok = do
  sp <- optional (match JSGFSpace)
  v <- match tok
  pure (v, sp)

matchText : Grammar state JSGFToken True (String, Maybe (TokType JSGFSpace))
matchText = do
  sp <- optional (match JSGFSpace)
  text <- forget <$> some (match JSGFText)
  pure ((foldl ((++)) "" text), sp)

matchTextDot : Grammar state JSGFToken True (String, Maybe (TokType JSGFSpace))
matchTextDot = do
  sp <- optional (match JSGFSpace)
  text <- forget <$> some (choice [match JSGFText, match JSGFDot])
  pure ((foldl ((++)) "" text), sp)

matchTextDotStar : Grammar state JSGFToken True (String, Maybe (TokType JSGFSpace))
matchTextDotStar = do
  sp <- optional (match JSGFSpace)
  text <- forget <$> some (choice [match JSGFText, match JSGFDot, match JSGFStar])
  pure ((foldl ((++)) "" text), sp)

matchKeyword : String -> Grammar state JSGFToken True (String, Maybe (TokType JSGFSpace))
matchKeyword keyword = do
  sp <- optional (match JSGFSpace)
  k <- match JSGFText
  when (k /= keyword) $ fail "Expected '\{keyword}', found '\{k}'"
  pure (k, sp)

selfIdent : Grammar state JSGFToken True SelfIdent
selfIdent = do
  signature <- matchsp JSGFSignature
  version   <- matchTextDot
  encoding  <- optional (matchsp JSGFText)
  locale    <- optional (matchsp JSGFText)
  semi      <- matchsp JSGFSemi
  pure (MkSelfIdent signature version encoding locale semi)

grammarName : Grammar state JSGFToken True GrammarName
grammarName = do
  keyword     <- matchKeyword "grammar"
  packageName <- matchTextDot
  semi        <- matchsp JSGFSemi
  pure (MkGrammarName keyword packageName semi)

import_ : Grammar state JSGFToken True Import
import_ = do
  keyword      <- matchKeyword "import"
  openBracket  <- matchsp JSGFLAngBracket
  packageName  <- matchTextDotStar
  closeBracket <- matchsp JSGFRAngBracket
  semi         <- matchsp JSGFSemi
  pure (MkImport keyword openBracket packageName closeBracket semi)

ruleName : Grammar state JSGFToken True RuleName
ruleName = do
  openBracket  <- matchsp JSGFLAngBracket
  ruleName     <- matchText
  closeBracket <- matchsp JSGFRAngBracket
  pure (MkRuleName openBracket ruleName closeBracket)

weight : Grammar state JSGFToken True Weight
weight = do
  value <- matchText
  when (check (fst value) == False) $ fail "Invalid weight"
  pure (MkWeight value)

  where
  check : String -> Bool
  check w = 
    let w' = unpack w
        h' = head' w'
        l' = last' w'
    in h' == l' && h' == Just '/'

ruleDef : Grammar state JSGFToken True RuleDef
ruleDef = do
  modifier     <- optional (matchKeyword "public")
  name         <- ruleName
  equals       <- matchsp JSGFEquals
  pure (MkRuleDef modifier name equals)

rule : Grammar state JSGFToken True Rule
rule = do
  def    <- ruleDef
  pure (MkRule def)

doc : Grammar state JSGFToken True Doc
doc = do 
  pure $ MkDoc 
    { selfIdent   = !selfIdent
    , grammarName = !grammarName
    , imports     = !(many import_)
    , rules       = !(many rule)
    }

export
jsgfParse : List (WithBounds JSGFToken) -> Either (List1 (ParsingError JSGFToken)) Doc
jsgfParse tokList = 
  case parse doc tokList of
    Right (r, Nil) => Right r
    Right (_, (MkBounded _ _ bounds)::_) => Left $ (Error "Document not fully parsed" (Just bounds)) ::: Nil
    Left err => Left err
