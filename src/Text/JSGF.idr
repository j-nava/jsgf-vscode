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
data Position : Type where
  MkPosition : (line : Int) -> (col : Int) -> Position

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
Show (Uri a) where
  show (MkUri s) = s

public export
FileData : Type
FileData = String

public export
record Context where
  constructor MkContext
  ruleNames : List String

public export
record Trees where
  constructor MkTrees
  concrete : C.Doc
  abstract : A.Tree

public export
record ParsedFile where
  constructor MkParsedFile
  uri     : Uri Absolute
  trees   : Trees
  context : Context

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
jsgfParse : String -> Result Trees
jsgfParse s = do
  doc <- parseDoc s
  tree <- docToTree doc
  pure $ MkTrees { concrete = doc, abstract = tree }

export
jsgfParseCurrent : 
  MonadError ErrorResult m => 
  ((currentFileLocation : Uri Absolute) -> (locationRelatedToCurrentFile : Uri Relative) -> m (Uri Absolute)) -> 
  (Uri Absolute -> m (Maybe FileData)) -> 
  (Uri Absolute, FileData) -> 
  ParsedFiles -> 
  m ParsedFiles
jsgfParseCurrent convertUriFn readUriTextFn (currentUri, currentFileData) =

  parseCurrentFile >=> findAndParseDependencies >=> pure . snd

  where
  hasUri : Uri Absolute -> ParsedFiles -> Bool
  hasUri uri pfs = isJust $ find (== uri) (.uri <$> pfs)

  upsertTrees : ParsedFiles -> ParsedFile -> ParsedFiles
  upsertTrees pfs pf = case hasUri pf.uri pfs of
    True  => (\pf' => if pf'.uri == pf.uri then { trees := pf.trees } pf' else pf') <$> pfs
    False => (pf :: pfs)

  parseCurrentFile : ParsedFiles -> m (ParsedFile, ParsedFiles)
  parseCurrentFile pfs = do
    trees <- liftEither (jsgfParse currentFileData)
    let pf = MkParsedFile { uri = currentUri, trees = trees, context = MkContext [] }
    pure (pf, upsertTrees pfs pf)

  findAndParseDependencies : (ParsedFile, ParsedFiles) -> m (ParsedFile, ParsedFiles)
  findAndParseDependencies (pf, pfs) = do
    deps <- fetchDeps
    pfs' <- foldlM parseDepIfNeeded pfs deps
    pure (pf, pfs')

    where
    parseDepIfNeeded : ParsedFiles -> Uri Relative -> m ParsedFiles
    parseDepIfNeeded pfs uri = do
      uriA <- convertUriFn currentUri uri
      case hasUri uriA pfs of
        True  => pure pfs
        False => do
          filedata <- readUriTextFn uriA
          case filedata of
            Just filedata' => jsgfParseCurrent convertUriFn readUriTextFn (uriA, filedata') pfs
            Nothing => throwError ((Error "Couldn't read file '\{show uriA}'" Nothing) ::: Nil)

    fetchDeps : m (List (Uri Relative))
    fetchDeps =

      execStateT [] $ A.traverseTree importFn ruleNameFn pf.trees.abstract

      where
      addDir : String -> StateT (List (Uri Relative)) m ()
      addDir pn =
        let
          dirs = forget $ split (== '.') pn 
          dir : Uri Relative
          dir = fromString $ (joinBy "/" dirs) ++ ".jsgf"
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

jsgfResolvePosition : MonadError ErrorResult m => Uri Absolute -> Position -> ParsedFiles -> m String
jsgfResolvePosition uri (MkPosition line col) pfs = case find (\pf => pf.uri == uri) pfs of
  Just pf => pure "GG"
  Nothing => throwError ((Error "File not parsed '\{show uri}'" Nothing) ::: Nil)

