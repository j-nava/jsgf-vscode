module Text.JSGF.Types

import Text.Bounded
import public Data.List1
import public Data.Fin
import Text.JSGF.Token

public export
record Ann where
  constructor MkAnn
  spaces    : String
  -- position  : Bounds

public export
PType : Type -> Type
PType a = (a, Ann)

public export
record WithBrackets (a : Type) where
  constructor MkWithBrackets
  openBracket  : PType JSGFBracketType
  value        : a
  closeBracket : PType JSGFBracketType

public export
record SelfIdent where
  constructor MkSelfIdent
  signature : PType ()
  version      : PType String
  charEncoding : Maybe (PType String)
  locale       : Maybe (PType String)
  semi         : PType ()

public export
record GrammarName where
  constructor MkGrammarName
  grammarKeyword : PType String
  packageName    : PType String
  semi           : PType ()

public export
record Import where
  constructor MkImport
  importKeyword : PType String
  packageName   : WithBrackets (PType String)
  semi          : PType ()

public export
record RuleName where
  constructor MkRuleName
  ruleName     : WithBrackets (PType String)

public export
record Weight where
  constructor MkWeight
  value   : PType String

public export
record RuleDef where
  constructor MkRuleDef
  modifier    : Maybe (PType String)
  ruleName    : RuleName
  tag         : Maybe (WithBrackets (PType String))
  equals      : PType String

public export
data RuleExpansion : Type where
  Token    : Maybe Weight -> PType String -> RuleExpansion
  Operator : PType String -> RuleExpansion
  RuleRef  : Maybe Weight -> (ruleName : WithBrackets (PType String)) -> RuleExpansion
  Group    : WithBrackets RuleExpansion -> RuleExpansion
  Tag      : WithBrackets (PType String) -> RuleExpansion
  Sequence : List1 RuleExpansion -> RuleExpansion

public export
record Rule where
  constructor MkRule
  ruleDef    : RuleDef
  expansion  : RuleExpansion
  semi       : PType ()

public export
record Doc where
  constructor MkDoc
  selfIdent     : SelfIdent
  grammarName   : GrammarName
  imports       : List Import
  rules         : List Rule
  finalSpace    : String
