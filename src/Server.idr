module Server

import Control.Monad.Either
import Control.Monad.Trans
import Data.IORef
import System.File.Types
import Text.JSGF

import Server.Common
import Server.Diagnostics
import Server.Autocomplete

%foreign "javascript:lambda:() => require('./server-ffi').load()"
export prim__load : PrimIO State
%foreign "javascript:lambda:(state) => require('./server-ffi').start(state)"
export prim__start : State -> PrimIO ()
%foreign "javascript:lambda:(state, f) => require('./server-ffi').onChangeConfig(state, f)"
export prim__onChangeConfig : State -> (State -> TextDocument -> IO ()) -> PrimIO ()
%foreign "javascript:lambda:(state, f) => require('./server-ffi').onChange(state, f)"
export prim__onChange : State -> (State -> TextDocument -> IO ()) -> PrimIO ()
%foreign "javascript:lambda:(state, f) => require('./server-ffi').onCompletion(state, f)"
prim__onCompletion : State -> (State -> String -> (line : Int) -> (col : Int) -> IO AnyPtr) -> PrimIO ()
%foreign "javascript:lambda:(document) => require('./server-ffi').getText(document)"
export prim__getText : TextDocument -> PrimIO String
%foreign "javascript:lambda:(document) => require('./server-ffi').getUri(document)"
export prim__getUri : TextDocument -> PrimIO String
%foreign "javascript:lambda:(curUri, relUri) => require('./server-ffi').getFullUri(curUri, relUri)"
export prim__getFullUri : (curUri : String) -> (relUri : String) -> PrimIO String
%foreign "javascript:lambda:(successFn, failVal, state, uri) => require('./server-ffi').getTextFromUri(successFn, failVal, state, uri)"
export prim__getTextFromUri : (String -> Maybe String) -> Maybe String -> State -> (uri : String) -> PrimIO (Maybe String)

getFullUri : HasIO io => Uri Absolute -> Uri Relative -> io (Uri Absolute)
getFullUri (MkUri curUri) (MkUri relUri) = primIO (prim__getFullUri curUri relUri) >>= pure . MkUri

getTextFromUri : HasIO io => State -> Uri Absolute -> io (Maybe FileData)
getTextFromUri state (MkUri uri) = primIO (prim__getTextFromUri Just Nothing state uri) 

export
onCompletion : HasIO io => State -> (State -> Uri Absolute -> Position -> IO CompletionItems) -> io ()
onCompletion state f = primIO (prim__onCompletion state (\state', uri, line, col => f state' (MkUri uri) (MkPosition line col)))

record ServerState where
  constructor MkServerState
  parsedFiles  : IORef (List ParsedFile)

processErrors : Diagnostics -> List1 (ParsingError (Token JSGFTokenKind)) -> IO ()
processErrors ds errors = traverse_ processError errors

  where
  processError : ParsingError (Token JSGFTokenKind) -> IO ()
  processError e@(Error message (Just bounds)) = do
    d <- primIO (prim__mkDiagnostic True (show e ++ "\n" ++ message) "Parser" bounds.startLine bounds.startCol bounds.endLine bounds.endCol)
    primIO (prim__pushDiagnostic ds d)
  processError e@(Error message Nothing) = do
    d <- primIO (prim__mkDiagnostic True (show e ++ "\n" ++ message) "Parser" 0 0 0 0)
    primIO (prim__pushDiagnostic ds d)

validate : ServerState -> State -> TextDocument -> IO ()
validate serverState state doc = do
  text <- primIO (prim__getText doc)
  ds <- primIO prim__mkDiagnostics
  pfs <- readIORef serverState.parsedFiles
  uri <- primIO (prim__getUri doc)
  let
    parsedFiles : EitherT ErrorResult IO ParsedFiles
    parsedFiles = jsgfParseCurrent getFullUri (getTextFromUri state) (fromString uri, text) pfs
  runEitherT parsedFiles >>= \case
    Right newPfs => writeIORef serverState.parsedFiles newPfs
    Left errors => processErrors ds errors
  primIO (prim__sendDiagnostics state doc ds)

  where
autocomplete : ServerState -> State -> Uri Absolute -> Position -> IO CompletionItems
autocomplete serverState state uri pos = do
  pfs <- readIORef serverState.parsedFiles
  items <- primIO (prim__mkCompletionItems)
  let
    getParsedFile : EitherT ErrorResult IO ParsedFile
    getParsedFile = jsgfGetParsedFile uri pfs
  runEitherT getParsedFile >>= \case
    Right pf => 
      let 
        addCompletion : ContextRule -> IO ()
        addCompletion rule = pushCompletionItem items Text rule.name "detail 2" "doc 2"
      in traverse_ addCompletion pf.context.rules
    Left errors => do
      ds <- primIO prim__mkDiagnostics
      processErrors ds errors
  pure items

main : IO ()
main = do
  state <- primIO (prim__load)
  parsedFiles <- newIORef []
  let serverState = MkServerState { parsedFiles = parsedFiles }
  primIO (prim__start state)
  primIO (prim__onChangeConfig state (validate serverState))
  primIO (prim__onChange state (validate serverState))
  onCompletion state (autocomplete serverState)

  -- async function validateTextDocument(textDocument: TextDocument): Promise<void> {
  --   const text = textDocument.getText();
  --   const diagnostics: Diagnostic[] = [];

  --   // var result = lib.parse(text);
  --   // if (!lib.isSuccessful(result)) {
  --   // 	const diagnostic: Diagnostic = {
  --   // 		severity: DiagnosticSeverity.Warning,
  --   // 		range: {
  --   // 			start: textDocument.positionAt(0),
  --   // 			end: textDocument.positionAt(0)
  --   // 		},
  --   // 		message: lib.getErrorMessage(result),
  --   // 		source: "ex"
  --   // 	};
  --   //   diagnostics.push(diagnostic);
  --   // }

  --   connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
  -- }