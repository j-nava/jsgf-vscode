module Server

import Control.Monad.Either
import Control.Monad.Trans
import Data.IORef
import System.File.Types
import Text.JSGF

import Server.Common
import Server.Diagnostics

%foreign "javascript:lambda:() => require('./server-ffi').load()"
export prim__load : PrimIO State
%foreign "javascript:lambda:(state) => require('./server-ffi').start(state)"
export prim__start : State -> PrimIO ()
%foreign "javascript:lambda:(state, f) => require('./server-ffi').onChangeConfig(state, f)"
export prim__onChangeConfig : State -> (State -> TextDocument -> IO ()) -> PrimIO ()
%foreign "javascript:lambda:(state, f) => require('./server-ffi').onChange(state, f)"
export prim__onChange : State -> (State -> TextDocument -> IO ()) -> PrimIO ()
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

record ServerState where
  constructor MkServerState
  parsedFiles  : IORef (List ParsedFile)

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
    Left errors => traverse_ (processError ds) errors
  primIO (prim__sendDiagnostics state doc ds)

  where
    processError : Diagnostics -> ParsingError (Token JSGFTokenKind) -> IO ()
    processError ds e@(Error message (Just bounds)) = do
      d <- primIO (prim__mkDiagnostic True (show e ++ "\n" ++ message) "Parser" bounds.startLine bounds.startCol bounds.endLine bounds.endCol)
      primIO (prim__pushDiagnostic ds d)
    processError ds e@(Error message Nothing) = do
      d <- primIO (prim__mkDiagnostic True (show e ++ "\n" ++ message) "Parser" 0 0 0 0)
      primIO (prim__pushDiagnostic ds d)

main : IO ()
main = do
  state <- primIO (prim__load)
  parsedFiles <- newIORef []
  let serverState = MkServerState { parsedFiles = parsedFiles }
  primIO (prim__start state)
  primIO (prim__onChangeConfig state (validate serverState))
  primIO (prim__onChange state (validate serverState))

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