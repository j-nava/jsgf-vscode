module Text.JSGF

import Control.Monad.Either
import Data.String
import Data.List
import Control.Monad.State

import public Text.JSGF.Parser.Token
import public Text.JSGF.Tree.Concrete as C
import public Text.JSGF.Tree.Abstract as A
import public Text.JSGF.Parser
import public Text.Parser

public export
data UriType = Absolute | Relative

public export
data Uri : (ut : UriType) -> Type where
  MkUri : String -> Uri ut
public export
FromString (Uri a) where
  fromString = MkUri
public export
Eq (Uri a) where
  (==) (MkUri x) (MkUri y) = x == y

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
  uri  : Uri Absolute
  jsgf : JSGF

public export
ParsedFiles : Type
ParsedFiles = List ParsedFile

export
docToTree : C.Doc -> Result A.Tree
docToTree doc = pure $ A.MkTree
  { packageName = fromCValue doc.grammarName.packageName
  , imports = !(traverse importFromString doc.imports)
  , rules = !(traverse rule doc.rules)
  }

  where
  fromCAnn : C.Ann -> A.Ann
  fromCAnn ann = A.MkAnn { position = ann.position }

  fromCValue : C.PType a -> A.TType a 
  fromCValue (value, ann) = (value, fromCAnn ann)

  importFromString : C.Import -> Result A.Import
  importFromString imp =
    case reverse $ trim <$> forget (split (== '.') (fst imp.packageName.value)) of
      (rn::nss)  => pure $ A.MkImport { packageName = (joinBy "." (reverse nss), fromCAnn $ snd imp.packageName.value), ruleName = if rn == "*" then A.AllGrammars else A.OneGrammar rn }
      Nil => Left $ singleton $ Error "Import: invalid package '\{fst imp.packageName.value}'" (Just (snd imp.packageName.value).position)

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
jsgfParseCurrent : MonadError ErrorResult m => (Uri Relative -> Uri Absolute) -> (Uri Absolute -> m FileData) -> (Uri Absolute, FileData) -> ParsedFiles -> m ParsedFiles
jsgfParseCurrent convertUriFn readUriTextFn (uri, filedata) =

  parseCurrentFile >=> findAndParseDependencies >=> pure . snd

  where
  hasUri : Uri Absolute -> ParsedFiles -> Bool
  hasUri uri pfs = isJust $ find (== uri) (.uri <$> pfs)

  upsertJSGF : ParsedFiles -> (Uri Absolute, JSGF) -> ParsedFiles
  upsertJSGF pfs (uri,jsgf) = case hasUri uri pfs of
    True  => (\pf => if pf.uri == uri then { jsgf := jsgf } pf else pf) <$> pfs
    False => ((MkParsedFile { uri = uri, jsgf = jsgf}) :: pfs)

  parseCurrentFile : ParsedFiles -> m (JSGF, ParsedFiles)
  parseCurrentFile pfs = do
    jsgf <- liftEither (jsgfParse filedata)
    pure (jsgf, upsertJSGF pfs (uri, jsgf))

  findAndParseDependencies : (JSGF, ParsedFiles) -> m (JSGF, ParsedFiles)
  findAndParseDependencies (jsgf, pfs) = do
    deps <- fetchDeps
    pfs' <- foldlM parseDepIfNeeded pfs deps
    pure (jsgf, pfs')

    where
    parseDepIfNeeded : ParsedFiles -> Uri Relative -> m ParsedFiles
    parseDepIfNeeded pfs uri = 
      let
        uriA = convertUriFn uri
      in
        case hasUri uriA pfs of
          True  => pure pfs
          False => do
            filedata <- readUriTextFn uriA
            jsgfParseCurrent convertUriFn readUriTextFn (uriA, filedata) pfs

    fetchDeps : m (List (Uri Relative))
    fetchDeps =

      execStateT [] $ A.traverseTree importFn ruleNameFn jsgf.abstract

      where
      addDir : String -> StateT (List (Uri Relative)) m ()
      addDir pn =
        let
          dirs = forget $ split (== '.') pn 
          dir : Uri Relative
          dir = fromString $ joinBy "/" dirs
        in modify ((::) dir)

      importFn : A.Import -> StateT (List (Uri Relative)) m A.Import
      importFn imp = do
        addDir (fst imp.packageName) 
        pure imp

      ruleNameFn : Bool -> RuleName -> StateT (List (Uri Relative)) m RuleName
      ruleNameFn True rn = do
        addDir (fst rn)
        pure rn
      ruleNameFn False rn = pure rn
