module Server

import Text.JSGF

State : Type
State = AnyPtr

TextDocument : Type
TextDocument = AnyPtr

%foreign "javascript:lambda:() => require('./server-ffi').load()"
export prim__load : PrimIO State
%foreign "javascript:lambda:(state) => require('./server-ffi').start(state)"
export prim__start : State -> PrimIO ()
%foreign "javascript:lambda:(state, f) => require('./server-ffi').onChangeConfig(state, f)"
export prim__onChangeConfig : State -> (State -> TextDocument -> IO ()) -> PrimIO ()
%foreign "javascript:lambda:(state, f) => require('./server-ffi').onChange(state, f)"
export prim__onChange : State -> (State -> TextDocument -> IO ()) -> PrimIO ()
%foreign "javascript:lambda:(document) => require('./server-ffi').getText()"
export prim__getText : TextDocument -> PrimIO String
%foreign "javascript:lambda:(state, document, message) => require('./server-ffi').sendDiagnostics(state, document, message)"
export prim__sendDiagnostics : State -> TextDocument -> (message : String) -> PrimIO ()

validate : State -> TextDocument -> IO ()
validate state doc = do
  text <- primIO (prim__getText doc)
  primIO (prim__sendDiagnostics state doc "blabla err")

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