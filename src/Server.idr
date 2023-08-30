module Server

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

validate : State -> TextDocument -> IO ()
validate state doc = do
  text <- primIO (prim__getText doc)
  ds <- primIO prim__mkDiagnostics
  case jsgfParse text of
    Left errors => do
      traverse_ (processError ds) errors
    Right jsgf => pure ()
  primIO (prim__sendDiagnostics state doc ds)

  where
    processError : Diagnostics -> ParsingError (Token JSGFTokenKind) -> IO ()
    processError ds e@(Error message (Just bounds)) = do
      d <- primIO (prim__mkDiagnostic True (show e) "Parser" bounds.startLine bounds.startCol bounds.endLine bounds.endCol)
      primIO (prim__pushDiagnostic ds d)
    processError ds e@(Error message Nothing) = do
      d <- primIO (prim__mkDiagnostic True (show e) "Parser" 0 0 0 0)
      primIO (prim__pushDiagnostic ds d)

main : IO ()
main = do
  state <- primIO (prim__load)
  primIO (prim__start state)
  primIO (prim__onChangeConfig state validate)
  primIO (prim__onChange state validate)

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