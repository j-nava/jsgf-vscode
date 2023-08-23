module Server.Common

public export
State : Type
State = AnyPtr

public export
TextDocument : Type
TextDocument = AnyPtr

%foreign "javascript:lambda:(state, message) => require('./server-ffi').showInformationMessage(state, message)"
export prim__showInformationMessage : State -> String -> PrimIO ()