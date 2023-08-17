module VSCode.Extension

import Text.JSGF

public export
ExtensionContext : Type
ExtensionContext = AnyPtr

%foreign "javascript:lambda:(context) => require('./client').activate(context)"
export prim__activate : ExtensionContext -> PrimIO ()
%foreign "javascript:lambda:() => require('./client').deactivate()"
export prim__deactivate : PrimIO ()

%export "javascript:activate"
activate : ExtensionContext -> ()
activate context = unsafePerformIO $ do
  primIO (prim__activate context)

%export "javascript:deactivate"
deactivate : ()
deactivate = unsafePerformIO $ do
  primIO prim__deactivate

main : IO ()
main = pure ()
