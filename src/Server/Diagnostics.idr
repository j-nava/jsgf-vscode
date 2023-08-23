module Server.Diagnostics

import Server.Common

public export
Diagnostic : Type
Diagnostic = AnyPtr

public export
Diagnostics : Type
Diagnostics = AnyPtr

%foreign "javascript:lambda:(isError, message, source, startLine, startCol, endLine, endCol) => require('./server-ffi').mkDiagnostic(isError, message, source, startLine, startCol, endLine, endCol)"
export prim__mkDiagnostic : Bool -> (message : String) -> (source : String) -> (startLine : Int) -> (startCol : Int) -> (endLine : Int) -> (endCol: Int) -> PrimIO Diagnostic

%foreign "javascript:lambda:() => require('./server-ffi').mkDiagnostics()"
export prim__mkDiagnostics : PrimIO Diagnostics

%foreign "javascript:lambda:(ds, d) => require('./server-ffi').pushDiagnostic(ds, d)"
export prim__pushDiagnostic : Diagnostics -> Diagnostic -> PrimIO ()

%foreign "javascript:lambda:(state, document, diagnostics) => require('./server-ffi').sendDiagnostics(state, document, diagnostics)"
export prim__sendDiagnostics : State -> TextDocument -> Diagnostics -> PrimIO ()
