module Server.Autocomplete

import Server.Common

public export
CompletionItems : Type
CompletionItems = AnyPtr

public export
CompletionItem : Type
CompletionItem = AnyPtr

%foreign "javascript:lambda:() => require('./server-ffi').mkCompletionItems()"
export prim__mkCompletionItems : PrimIO CompletionItems

%foreign "javascript:lambda:(cs, label, detail, documentation) => require('./server-ffi').pushCompletionItem(cs, label, detail, documentation)"
export prim__pushCompletionItem : CompletionItems -> String -> String -> String -> PrimIO ()
