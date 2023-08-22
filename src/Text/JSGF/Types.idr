module Text.JSGF.Types

import public Data.List1
import public Data.Fin

public export
Marker : Type 
Marker = String

public export
data MDInline
  = Textual String
  | Strong String
  | Emphasis String
  | Code String
  | Link String String

public export
data MDBlockItem = AnyBlockItem | ListItemBlockItem

public export
data MDBlock : {0 blockItem : MDBlockItem} -> Type where
  ThematicBreak : MDBlock { blockItem = AnyBlockItem }
  Paragraph     : List1 MDInline -> MDBlock { blockItem = AnyBlockItem }
  Heading       : (Fin 6) -> List1 MDInline -> MDBlock { blockItem = AnyBlockItem }
  BlankLine     : Nat -> MDBlock { blockItem = AnyBlockItem }
  IListItem     : List1 (MDBlock { blockItem = AnyBlockItem }) -> MDBlock { blockItem = ListItemBlockItem }
  IList         : Marker -> List1 (MDBlock { blockItem = ListItemBlockItem }) -> MDBlock { blockItem = AnyBlockItem }
  Indentation   : Nat -> List1 MDInline -> MDBlock { blockItem = AnyBlockItem }

public export
PType : Type -> Type
PType a = (a, Maybe String)

public export
record SelfIdent where
  constructor MkSelfIdent
  version : PType String
  charEncoding : Maybe (PType String)
  locale : Maybe (PType String)

public export
data Block : Type where
  BSelfIdent : SelfIdent -> Block

public export
Doc : Type
Doc = List1 Block
