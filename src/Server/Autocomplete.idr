module Server.Autocomplete

import Server.Common

public export
CompletionItems : Type
CompletionItems = AnyPtr

public export
data CompletionKind = Function | Text

Show CompletionKind where
  show Function = "function"
  show Text = "text"

%foreign "javascript:lambda:() => require('./server-ffi').mkCompletionItems()"
export prim__mkCompletionItems : PrimIO CompletionItems

%foreign "javascript:lambda:(cs, kind, label, detail, documentation, isShadow) => require('./server-ffi').pushCompletionItem(cs, kind, label, detail, documentation, isShadow)"
prim__pushCompletionItem : CompletionItems -> String -> String -> String -> String -> Bool -> PrimIO ()

export
pushCompletionItem : HasIO io => CompletionItems -> CompletionKind -> (label : String) -> (detail : String) -> (description : String) -> (isShadow : Bool) -> io ()
pushCompletionItem cs kind label detail description isShadow = primIO (prim__pushCompletionItem cs (show kind) label detail description isShadow)
