module Text.JSGF

import Control.Monad.Either
import Data.List

import public Text.JSGF.Tree.Concrete as C
import public Text.JSGF.Tree.Abstract as A
import public Text.JSGF.Parser
import public Text.Parser
import public Text.JSGF.Parser.Token

public export
data URIType = Absolute | Relative

public export
data URI : (ut : URIType) -> Type where
  MkURI : String -> URI ut
public export
FromString (URI a) where
  fromString = MkURI
public export
Eq (URI a) where
  (==) (MkURI x) (MkURI y) = x == y

public export
FileData : Type
FileData = String

public export
record JSGF where
  constructor MkJSGF
  concrete : C.Doc
  abstract : A.Tree

public export
record ParsedFile where
  constructor MkParsedFile
  uri  : URI Absolute
  jsgf : JSGF

public export
ParsedFiles : Type
ParsedFiles = List ParsedFile

export
docToTree : C.Doc -> Result A.Tree
docToTree doc = pure $ A.MkTree
  { packageName = fromCValue doc.grammarName.packageName
  , rules = !(traverse rule doc.rules)
  }

  where
  fromCAnn : C.Ann -> A.Ann
  fromCAnn ann = A.MkAnn { position = ann.position }

  fromCValue : C.PType a -> A.TType a 
  fromCValue (value, ann) = (value, fromCAnn ann)

  ruleExpansion : C.RuleExpansion -> Result A.RuleExpr
  ruleExpansion (C.Token _ token) = pure $ A.Token (fromCValue token)
  ruleExpansion (C.Tag tag) = pure $ A.Tag (fromCValue tag.value)
  ruleExpansion (C.Operator ("*", ann)) = pure $ A.UnaryOp (A.KleeneStar, fromCAnn ann)
  ruleExpansion (C.Operator ("+", ann)) = pure $ A.UnaryOp (A.PlusOperator, fromCAnn ann)
  ruleExpansion (C.Operator ("|", ann)) = pure $ A.Alternative (fromCAnn ann)
  ruleExpansion (C.Operator (op, ann)) = Left $ singleton $ Error "Invalid operator '\{op}'" (Just ann.position)
  ruleExpansion (C.RuleRef _ name) = pure $ A.RuleRef (fromCValue name.value)
  ruleExpansion (C.Group (MkWithBrackets { openBracket = (JSGFSquareBracket,_), value, closeBracket })) = pure $ A.Group A.OptionalGrouping !(ruleExpansion value)
  ruleExpansion (C.Group (MkWithBrackets { openBracket = (JSGFParens,_), value, closeBracket })) = pure $ A.Group A.RequiredGrouping !(ruleExpansion value)
  ruleExpansion (C.Group (MkWithBrackets { openBracket, value, closeBracket })) = Left $ singleton $ Error "Invalid rule expansion grouping" (Just (snd openBracket).position)
  ruleExpansion (C.Sequence res) = A.Sequence <$> traverse ruleExpansion res

  rule : C.Rule -> Result A.Rule
  rule cr = pure $ A.MkRule { name = fromCValue cr.ruleDef.ruleName.value, isPublic = maybe False ((==) "public" . fst) cr.ruleDef.modifier, expr = !(ruleExpansion cr.expansion) }

export
jsgfParse : String -> Result JSGF
jsgfParse s = do
  doc <- parseDoc s
  tree <- docToTree doc
  pure $ MkJSGF { concrete = doc, abstract = tree }

export
jsgfParseCurrent : MonadError ErrorResult m => (URI Relative -> m (URI Absolute, FileData)) -> (URI Absolute, FileData) -> ParsedFiles -> m ParsedFiles
jsgfParseCurrent readFileTextFn (uri,filedata) =

  parseCurrentFile >=> parseDependencies

  where
  parseCurrentFile : ParsedFiles -> m ParsedFiles
  parseCurrentFile pfs = do
    jsgf <- liftEither (jsgfParse filedata)
    pure (upsertJSGF jsgf)
    where
    upsertJSGF : JSGF -> ParsedFiles
    upsertJSGF jsgf = case isJust $ find (== uri) (.uri <$> pfs) of
      True  => (\pf => if pf.uri == uri then { jsgf := jsgf } pf else pf) <$> pfs
      False => ((MkParsedFile { uri = uri, jsgf = jsgf}) :: pfs)

  parseDependencies : ParsedFiles -> m ParsedFiles
  parseDependencies pfs = ?parseDependencies_rhs
