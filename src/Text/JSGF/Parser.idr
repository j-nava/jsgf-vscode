module Text.JSGF.Parser

import Data.String
import Text.Parser
import Text.JSGF.Token
import Text.JSGF.Types

matchtok : TokenKind JSGFTokenKind => Eq JSGFTokenKind => (kind : JSGFTokenKind) -> Grammar state (Token JSGFTokenKind) True (TokType kind, String)
matchtok tok = do
  v <- match tok
  pure (v, "")

matchsp : Grammar state (Token JSGFTokenKind) False String
matchsp = do
  sp <- many (match JSGFSpace)
  pure (foldl (++) "" sp)

withSpace' : Lazy (Grammar state (Token JSGFTokenKind) True String) -> Grammar state (Token JSGFTokenKind) True (String, String)
withSpace' parser = do
  sp <- matchsp
  text <- some parser 
  pure ((foldl ((++)) "" text), sp)

withSpace : TokenKind JSGFTokenKind => Eq JSGFTokenKind => (kind : JSGFTokenKind) -> Grammar state (Token JSGFTokenKind) True (TokType kind, String)
withSpace tok = do
  sp <- matchsp
  v <- match tok
  pure (v, sp)

between' : List JSGFBracketType -> Lazy (Grammar state JSGFToken True a) -> Grammar state JSGFToken True (WithBrackets a)
between' bracketType parser = do
  openB  <- withSpace JSGFOpen
  when (isNothing $ find (== fst openB) bracketType) $ fail "Unexpected bracket type"
  value  <- parser
  closeB <- withSpace JSGFClose
  when ((fst closeB) /= (fst openB)) $ fatalError "Expected '\{show $ fst openB}', found '\{show $ fst closeB}'"
  pure $ MkWithBrackets { openBracket = openB, value = value, closeBracket = closeB }

matchText : Grammar state JSGFToken True (String, String)
matchText = withSpace' (match JSGFText)

matchTextDot : Grammar state JSGFToken True (String, String)
matchTextDot = withSpace' (choice [match JSGFText, match JSGFDot])

matchTextDotStar : Grammar state JSGFToken True (String, String)
matchTextDotStar = withSpace' (choice [match JSGFText, match JSGFDot, match JSGFStar])

matchTextOperator : Grammar state JSGFToken True (String, String)
matchTextOperator = withSpace' (choice [match JSGFStar, match JSGFPlus, match JSGFPipe])

matchTextTag : Grammar state JSGFToken True (String, String)
matchTextTag = withSpace' (choice [match JSGFText, match JSGFDot, match JSGFStar, match JSGFPlus, match JSGFPipe])

matchKeyword : String -> Grammar state JSGFToken True (String, String)
matchKeyword keyword = do
  sp <- matchsp
  k <- match JSGFText
  when (k /= keyword) $ fail "Expected '\{keyword}', found '\{k}'"
  pure (k, sp)

selfIdent : Grammar state JSGFToken True SelfIdent
selfIdent = do
  signature <- withSpace JSGFSignature
  version   <- matchTextDot
  encoding  <- optional (withSpace JSGFText)
  locale    <- optional (withSpace JSGFText)
  semi      <- withSpace JSGFSemi
  pure (MkSelfIdent signature version encoding locale semi)

grammarName : Grammar state JSGFToken True GrammarName
grammarName = do
  keyword     <- matchKeyword "grammar"
  packageName <- matchTextDot
  semi        <- withSpace JSGFSemi
  pure (MkGrammarName keyword packageName semi)

import_ : Grammar state JSGFToken True Import
import_ = do
  keyword      <- matchKeyword "import"
  packageName  <- between' [JSGFAngBracket] matchTextDotStar
  semi         <- withSpace JSGFSemi
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
  tag          <- optional (between' [JSGFCurlyBracket] matchTextTag)
  equals       <- withSpace JSGFEquals
  pure (MkRuleDef modifier name tag equals)

mutual
  ruleExpansion : Grammar state JSGFToken True RuleExpansion
  ruleExpansion = do
    res   <- some (choice [ruleExpansionGroup, ruleExpansionOperator, ruleExpansionRuleRef, ruleExpansionTag, ruleExpansionToken])
    pure $ case res of
      (x:::Nil) => x
      xs => Sequence xs

  ruleExpansionGroup : Grammar state JSGFToken True RuleExpansion
  ruleExpansionGroup = do
    ruleName     <- between' [JSGFParens, JSGFSquareBracket] ruleExpansion
    pure (Group ruleName)

  ruleExpansionTag : Grammar state JSGFToken True RuleExpansion
  ruleExpansionTag = do
    tag     <- between' [JSGFCurlyBracket] matchTextTag
    pure (Tag tag)

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
  semi      <- withSpace JSGFSemi
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
