module Server

import Text.JSGF

State : Type
State = AnyPtr

-- %foreign "javascript:lambda:(context) => require('./client').activate(context)"
-- export prim__activate : ExtensionContext -> PrimIO ()
%foreign "javascript:lambda:() => require('./server-ffi').load()"
export prim__load : PrimIO State
%foreign "javascript:lambda:(state) => require('./server-ffi').start(state)"
export prim__start : State -> PrimIO ()

-- %export "javascript:activate"
-- start : ExtensionContext -> ()
-- start context = unsafePerformIO $ do
--   primIO (prim__activate context)

-- %export "javascript:deactivate"
-- deactivate : ()
-- deactivate = unsafePerformIO $ do
--   primIO prim__deactivate

main : IO ()
main = do
  state <- primIO (prim__load)
  primIO (prim__start state)
