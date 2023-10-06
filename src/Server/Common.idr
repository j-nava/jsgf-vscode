module Server.Common

import Text.JSGF

public export
State : Type
State = AnyPtr

public export
TextDocument : Type
TextDocument = AnyPtr

%foreign "javascript:lambda:(state, message) => require('./server-ffi').showInformationMessage(state, message)"
export prim__showInformationMessage : State -> String -> PrimIO ()

%foreign "javascript:lambda:(fromPath, toPath) => require('./server-ffi').getRelativePath(fromPath, toPath)"
prim__getRelativePath : String -> String -> String

export
getRelativePath : Uri Absolute -> Uri Absolute -> Uri Relative
getRelativePath fromPath toPath = MkUri $ prim__getRelativePath (show fromPath) (show toPath)
