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
data ImportRuleName = AllGrammars | OneGrammar String

public export
record Import where
  constructor MkImport
  packageName : TType String
  ruleName    : ImportRuleName

public export
RuleName : Type
RuleName = TType String

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
  RuleRef     : RuleName -> RuleExpr
  Group       : GroupType -> RuleExpr -> RuleExpr
  Sequence    : List1 RuleExpr -> RuleExpr

public export
record Rule where
  constructor MkRule
  name     : RuleName
  isPublic : Bool
  expr     : RuleExpr

public export
record Tree where
  constructor MkTree
  packageName  : TType String
  imports      : List Import
  rules        : List Rule


export
traverseTree : Monad m => (Import -> m Import) -> ((isRef : Bool) -> RuleName -> m RuleName) -> Tree -> m Tree
traverseTree iFn rnFn tree = pure $ 
  { imports := !(traverse traverseImport tree.imports)
  , rules   := !(traverse traverseRule tree.rules) 
  } tree

  where
  traverseImport : Import -> m Import
  traverseImport = iFn

  traverseRuleExpr : RuleExpr -> m RuleExpr
  traverseRuleExpr (RuleRef rn)     = RuleRef <$> rnFn True rn
  traverseRuleExpr (Group gt expr)  = Group <$> pure gt <*> traverseRuleExpr expr
  traverseRuleExpr (Sequence exprs) = Sequence <$> traverse traverseRuleExpr exprs
  traverseRuleExpr re               = pure re

  traverseRule : Rule -> m Rule
  traverseRule r = pure $ { name := !(rnFn False r.name) } r
