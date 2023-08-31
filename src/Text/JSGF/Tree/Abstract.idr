module Text.JSGF.Tree.Abstract

import Data.List1
import Text.Bounded

public export
record Ann where
  constructor MkAnn
  position  : Bounds

public export
TType : Type -> Type 
TType a = (a, Ann)

public export
data UnaryOperator = KleeneStar | PlusOperator

public export
data GroupType = RequiredGrouping | OptionalGrouping 

public export
data RuleExpr : Type where
  Token       : TType String -> RuleExpr
  Tag         : TType String -> RuleExpr
  UnaryOp     : TType UnaryOperator -> RuleExpr
  Alternative : Ann -> RuleExpr
  RuleRef     : TType String -> RuleExpr
  Group       : GroupType -> RuleExpr -> RuleExpr
  Sequence    : List1 RuleExpr -> RuleExpr

public export
record Rule where
  constructor MkRule
  name     : TType String
  isPublic : Bool
  expr     : RuleExpr

public export
record Tree where
  constructor MkTree
  packageName  : TType String
  imports      : List (TType String)
  rules        : List Rule
