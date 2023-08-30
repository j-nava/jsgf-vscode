module Text.JSGF.Tree.Concrete

import Text.Bounded
import public Data.List1
import public Data.Fin
import Text.JSGF.Parser.Token

public export
record Ann where
  constructor MkAnn
  spaces    : String
  position  : Bounds

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
  signature    : PType ()
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
Weight : Type
Weight = PType String

public export
record RuleDef where
  constructor MkRuleDef
  modifier    : Maybe (PType String)
  ruleName    : WithBrackets (PType String)
  tag         : Maybe (WithBrackets (PType String))
  equals      : PType String

public export
data RuleExpansion : Type where
  Token    : (weight : Maybe (PType String)) -> PType String -> RuleExpansion
  Operator : PType String -> RuleExpansion
  RuleRef  : (weight : Maybe (PType String)) -> (ruleName : WithBrackets (PType String)) -> RuleExpansion
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
