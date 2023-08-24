module Text.JSGF.Tree.Abstract

import Text.Bounded

public export
record Ann where
  constructor MkAnn
  position  : Bounds

public export
TType : Type -> Type 
TType a = (a, Ann)

data RuleExpr : Type where
  Token : TType String -> RuleExpr
  Unary : RuleExpr
  Binary : RuleExpr

record Rule where
  constructor MkRule
  name    : String

public export
Tree : Type