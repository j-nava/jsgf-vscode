module Text.JSGF.Types

import public Data.List1
import public Data.Fin

public export
PType : Type -> Type
PType a = (a, Maybe String)

public export
record SelfIdent where
  constructor MkSelfIdent
  signature : PType ()
  version : PType String
  charEncoding : Maybe (PType String)
  locale : Maybe (PType String)
  semi : PType ()

public export
record GrammarName where
  constructor MkGrammarName
  grammarKeyword : PType String
  packageName : PType String
  semi : PType ()

public export
record Import where
  constructor MkImport
  importKeyword : PType String
  openBracket : PType ()
  packageName : PType String
  closeBracket : PType ()
  semi : PType ()

public export
record RuleName where
  constructor MkRuleName
  openBracket  : PType ()
  ruleName     : PType String
  closeBracket : PType ()

public export
record Weight where
  constructor MkWeight
  value   : PType String

public export
record RuleDef where
  constructor MkRuleDef
  modifier    : Maybe (PType String)
  ruleName    : RuleName
  equals      : PType ()

public export
record RuleExpansion where
  constructor MkRuleExpansion
  a : Int

public export
record Rule where
  constructor MkRule
  ruleDef    : RuleDef

public export
record Doc where
  constructor MkDoc
  selfIdent     : SelfIdent
  grammarName   : GrammarName
  imports       : List Import
  rules         : List Rule
