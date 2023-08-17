module Server

import Text.JSGF

-- %foreign "javascript:lambda:(context) => require('./client').activate(context)"
-- export prim__activate : ExtensionContext -> PrimIO ()
%foreign "javascript:lambda:() => require('./server-ffi')"
export prim__start : PrimIO ()

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
  primIO (prim__start)
