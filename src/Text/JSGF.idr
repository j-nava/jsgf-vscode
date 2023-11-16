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
record ContextRule where
  constructor MkContextRule
  name       : String
  uri        : Uri Absolute
  importedBy : Uri Absolute
  isPublic   : Bool
  position   : Bounds

  isDup      : Bool
  isShadow   : Bool

public export
record WithOrigin (a : Type) where
  constructor MkWithOrigin
  position : Maybe Bounds 
  value    : a

public export
Eq ContextRule where
  (==) cr1 cr2 = cr1.name == cr2.name && cr1.uri == cr2.uri && cr1.importedBy == cr2.importedBy

public export
record Context where
  constructor MkContext
  rules : List ContextRule

public export
record Trees where
  constructor MkTrees
  concrete : C.Doc
  abstract : A.Tree

public export
record ParsedFile where
  constructor MkParsedFile
  uri     : Uri Absolute
  result  : Result Trees
  context : Maybe Context

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
jsgfGetParsedFile : MonadError ErrorResult m => ParsedFiles -> Uri Absolute -> m ParsedFile
jsgfGetParsedFile pfs uri = case find (\pf => pf.uri == uri) pfs of
  Just pf => pure pf
  Nothing => throwError ((Error "File not parsed '\{show uri}'" Nothing) ::: Nil)

export
jsgfParseCurrent : 
  MonadError ErrorResult m => 
  (convertUriFn : ((currentFileLocation : Uri Absolute) -> (locationRelatedToCurrentFile : Uri Relative) -> m (Uri Absolute))) -> 
  (readUriTextFn : (Uri Absolute -> m (Maybe FileData))) -> 
  (currentFileInfo : (Uri Absolute, FileData)) -> 
  ParsedFiles -> 
  m ParsedFiles
jsgfParseCurrent convertUriFn readUriTextFn (currentUri, currentFileData) =

  parseWithDependencies >=> buildAllContext -- >=> generateErrors -->=> validateContext

  where
  hasUri : Uri Absolute -> ParsedFiles -> Bool
  hasUri uri pfs = isJust $ find (== uri) (.uri <$> pfs)

  upsertParsedFiles : ParsedFiles -> ParsedFile -> ParsedFiles
  upsertParsedFiles pfs pf = case hasUri pf.uri pfs of
    True  => (\pf' => if pf'.uri == pf.uri then pf else pf') <$> pfs
    False => (pf :: pfs)

  fetchDeps : A.Tree -> List (WithOrigin (Uri Relative))
  fetchDeps tree =

    runIdentity $ execStateT [] $ A.traverseTree importFn ruleNameFn tree

    where

    addDir : A.TType String -> StateT (List (WithOrigin (Uri Relative))) Identity ()
    addDir pn =
      let
        dirs = forget $ split (== '.') (fst pn)
        dir : Uri Relative
        dir = fromString $ (joinBy "/" dirs) ++ ".jsgf"
      in modify ((::) (MkWithOrigin { value = dir, position = Just (snd pn).position }))

    importFn : A.Import -> StateT (List (WithOrigin (Uri Relative))) Identity A.Import
    importFn imp = do
      addDir imp.packageName
      pure imp

    ruleNameFn : Bool -> RuleName -> StateT (List (WithOrigin (Uri Relative))) Identity RuleName
    ruleNameFn True rn = do
      addDir rn
      pure rn
    ruleNameFn False rn = pure rn

  fetchDepsA : Uri Absolute -> A.Tree -> m (List (WithOrigin (Uri Absolute)))
  fetchDepsA baseUri tree =
    let urisR = fetchDeps tree
    in traverse (\u => convertUriFn baseUri u.value >>= (\u' => pure $ MkWithOrigin { value = u', position = u.position })) urisR

  invalidateContext : Uri Absolute -> ParsedFile -> ParsedFile
  invalidateContext uri pf with (pf.context)
    invalidateContext uri pf | Just context =
      case find (\rule => rule.uri == uri) context.rules of
        Just _ => { context := Nothing } pf
        Nothing => pf 
    invalidateContext _ pf | Nothing = pf

  buildContext : ParsedFiles -> ParsedFile -> m ParsedFile
  buildContext pfs pf with (pf.context) 
    buildContext _ pf | Just context = pure pf 
    buildContext pfs pf | Nothing = do
      rules <- findRules pfs pf.uri pf
      let rules' = runIdentity $ evalStateT [] $ traverse validateRule rules
      pure $ MkParsedFile { uri = pf.uri, result = pf.result, context = Just $ MkContext { rules = rules' } }

      where
      validateRule : ContextRule -> StateT (List ContextRule) Identity ContextRule
      validateRule rule = do
        rules <- get
        let isDupl = isJust $ find ((==) rule) rules
        let isShadow = isJust $ find (\r => r.name == rule.name && r.uri /= rule.uri) rules
        if isDupl
          then pure $ { isDup := True } rule
          else if isShadow
            then pure $ { isShadow := True } rule
            else do
              put $ rules `snoc` rule
              pure rule

      convertRule : Uri Absolute -> Uri Absolute -> A.Rule -> ContextRule
      convertRule importedBy uri rule = MkContextRule { name = fst rule.name, uri = uri, importedBy = importedBy, isPublic = rule.isPublic, position = (snd rule.name).position, isDup = False, isShadow = False }

      findRules : ParsedFiles -> Uri Absolute -> ParsedFile -> m (List ContextRule)
      findRules pfs importedBy pf = case pf.result of
        Left e => pure []
        Right trees => do
          let pfRules = convertRule importedBy pf.uri <$> trees.abstract.rules
          urisA <- fetchDepsA pf.uri trees.abstract
          deps <- traverse (jsgfGetParsedFile pfs) (.value <$> urisA)
          depRules <- foldlM (\a, pf' => findRules pfs pf.uri pf' <&> (\rs => a ++ rs)) [] deps
          pure . nubBy (\r1, r2 => r1.name == r2.name && r1.uri == r2.uri && r1.importedBy /= r2.importedBy) $ pfRules `Prelude.Types.List.(++)` depRules

  generateErrors : ParsedFiles -> m ParsedFiles
  generateErrors pfs = do
    pf <- jsgfGetParsedFile pfs currentUri
    case pf.result of
      Left e => throwError e
      Right trees => do
        urisA <- fetchDepsA currentUri trees.abstract
        errors <- foldlM (\a, c => generateDepErrors c >>= pure . (++) a) [] urisA
        case errors of
          [] => pure pfs
          (x::xs) => throwError (x:::xs)

    where
    generateDepErrors : WithOrigin (Uri Absolute) -> m (List (ParsingError JSGFToken))
    generateDepErrors uri = do
      pf <- jsgfGetParsedFile pfs uri.value
      case pf.result of
        Left errors => pure (forget errors)
        Right trees => pure []

    go : ParsedFile -> StateT (List (ParsingError JSGFToken)) m ParsedFile
    go pf = do
      case pf.result of
        Left errors => do
          modify $ (++) (forget errors)
          pure pf
        Right _ => pure pf

  buildAllContext : ParsedFiles -> m ParsedFiles
  buildAllContext pfs = do
    -- Important: for performance reasons, this function should be kept idempotent
    let pfs' = invalidateContext currentUri <$> pfs
    traverse (buildContext pfs') pfs'

  parseWithDependencies : ParsedFiles -> m ParsedFiles
  parseWithDependencies pfs = do
    execStateT pfs (go (Just currentFileData) (MkWithOrigin { value = currentUri, position = Nothing }))

    where

    go : Maybe FileData -> WithOrigin (Uri Absolute) -> StateT ParsedFiles m ()
    go filedata uri = do
      pfs <- get
      let isParsed = hasUri uri.value pfs 
      filedata' <- case filedata of
        Just fd => pure fd
        Nothing => do
          lift (readUriTextFn uri.value) >>= \case
            Just fd => pure fd
            Nothing => throwError ((Error "Couldn't read file '\{show uri.value}'" Nothing) ::: Nil)

      when (isJust filedata || not isParsed) $ do
        case jsgfParse filedata' of
          Left e => do
            let pf = MkParsedFile { uri = uri.value, result = Left e, context = Nothing }
            put (upsertParsedFiles pfs pf)
            throwError e
          Right trees => do
            let pf = MkParsedFile { uri = uri.value, result = Right trees, context = Nothing }
            put (upsertParsedFiles pfs pf)
            urisA <- lift $ fetchDepsA uri.value trees.abstract 
            traverse_ (go Nothing) urisA

  validateContext : ParsedFile -> m ParsedFile
  validateContext = pure . id -- TODO: 1. validate undeclared rules; 2. validate unused rules (warning); 3. validate private rules (diff error msg)

jsgfResolvePosition : MonadError ErrorResult m => Uri Absolute -> Position -> ParsedFiles -> m String
jsgfResolvePosition uri (MkPosition line col) pfs = do
  pf <- jsgfGetParsedFile pfs uri
  pure "" -- TODO

