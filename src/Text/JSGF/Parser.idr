module Text.JSGF.Parser

import Data.String
import Text.Parser
import Text.JSGF.Token
import Text.JSGF.Types

matchtok : TokenKind JSGFTokenKind => Eq JSGFTokenKind => (kind : JSGFTokenKind) -> Grammar state (Token JSGFTokenKind) True (TokType kind, Maybe (TokType JSGFSpace))
matchtok tok = do
  v <- match tok
  pure (v, Nothing)

matchsp' : Grammar state (Token JSGFTokenKind) True String -> Grammar state (Token JSGFTokenKind) True (String, Maybe (TokType JSGFSpace))
matchsp' parser = do
  sp <- optional (match JSGFSpace)
  text <- some parser 
  pure ((foldl ((++)) "" text), sp)

matchsp : TokenKind JSGFTokenKind => Eq JSGFTokenKind => (kind : JSGFTokenKind) -> Grammar state (Token JSGFTokenKind) True (TokType kind, Maybe (TokType JSGFSpace))
matchsp tok = do
  sp <- optional (match JSGFSpace)
  v <- match tok
  pure (v, sp)

between' : List JSGFBracketType -> Grammar state JSGFToken True a -> Grammar state JSGFToken True (WithBrackets a)
between' bracketType parser = do
  openB  <- matchsp JSGFOpen
  when (isNothing $ find (== fst openB) bracketType) $ fail "Unexpected bracket type"
  value  <- parser
  closeB <- matchsp JSGFClose
  when ((fst closeB) /= (fst openB)) $ fail "Unexpected bracket type"
  pure $ MkWithBrackets { openBracket = openB, value = value, closeBracket = closeB }

matchText : Grammar state JSGFToken True (String, Maybe (TokType JSGFSpace))
matchText = matchsp' (match JSGFText)

matchTextDot : Grammar state JSGFToken True (String, Maybe (TokType JSGFSpace))
matchTextDot = matchsp' (choice [match JSGFText, match JSGFDot])

matchTextDotStar : Grammar state JSGFToken True (String, Maybe (TokType JSGFSpace))
matchTextDotStar = matchsp' (choice [match JSGFText, match JSGFDot, match JSGFStar])

matchTextOperator : Grammar state JSGFToken True (String, Maybe (TokType JSGFSpace))
matchTextOperator = matchsp' (choice [match JSGFStar, match JSGFPlus, match JSGFPipe])

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
  packageName  <- between' [JSGFAngBracket] matchTextDotStar
  semi         <- matchsp JSGFSemi
  pure (MkImport keyword packageName semi)

ruleName : Grammar state JSGFToken True RuleName
ruleName = do
  ruleName     <- between' [JSGFAngBracket] matchText
  pure (MkRuleName ruleName)

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

mutual
  ruleExpansion : Grammar state JSGFToken True RuleExpansion
  ruleExpansion = do
    res   <- some (choice [ruleExpansionGroup, ruleExpansionOperator, ruleExpansionRuleRef, ruleExpansionToken])
    pure $ case res of
      (x:::Nil) => x
      xs => Sequence xs

  ruleExpansionGroup : Grammar state JSGFToken True RuleExpansion
  ruleExpansionGroup = do
    -- ruleName     <- between' [JSGFParens, JSGFSquareBracket] ruleExpansion
    ruleName     <- between' [JSGFParens, JSGFSquareBracket] ruleExpansionToken
    pure (Group ruleName)

  ruleExpansionOperator : Grammar state JSGFToken True RuleExpansion
  ruleExpansionOperator = do
    operator     <- matchTextOperator
    pure (Operator operator)

  ruleExpansionRuleRef : Grammar state JSGFToken True RuleExpansion
  ruleExpansionRuleRef = do
    weight       <- optional weight
    ruleName     <- between' [JSGFAngBracket] matchText
    pure (RuleRef weight ruleName)

  ruleExpansionToken : Grammar state JSGFToken True RuleExpansion
  ruleExpansionToken = do
    weight    <- optional weight
    token     <- matchText
    pure (Token weight token)

rule : Grammar state JSGFToken True Rule
rule = do
  def       <- ruleDef
  expansion <- ruleExpansion
  semi      <- matchsp JSGFSemi
  pure (MkRule def expansion semi)

doc : Grammar state JSGFToken True Doc
doc = do 
  pure $ MkDoc 
    { selfIdent   = !selfIdent
    , grammarName = !grammarName
    , imports     = !(many import_)
    , rules       = !(many rule)
    , finalSpace  = !(optional (match JSGFSpace))
    }

export
jsgfParse : List (WithBounds JSGFToken) -> Either (List1 (ParsingError JSGFToken)) Doc
jsgfParse tokList = 
  case parse doc tokList of
    Right (r, Nil) => Right r
    Right (_, (MkBounded _ _ bounds)::_) => Left $ (Error "Document not fully parsed" (Just bounds)) ::: Nil
    Left err => Left err
