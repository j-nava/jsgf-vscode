module Text.JSGF.Print

import Data.String
import Text.PrettyPrint.Bernardy

import Text.JSGF.Types

Pretty MDInline where
  prettyPrec _ = \case
    Textual text => line text
    Strong text => enclose "**" "**" (line text)
    Emphasis text => enclose "*" "*" (line text)
    Code text => enclose "`" "`" (line text)
    Link text url => brackets (line text) <+> parens (line url)

Pretty MDBlock where
  prettyPrec _ = \case
    ThematicBreak => line "---"
    Paragraph inline => hcat (pretty <$> forget inline)
    Heading level inline => line (replicate (finToNat level) '#') <++> hcat (pretty <$> forget inline)
    BlankLine count => flush ""
    IListItem blocks => hcat (pretty <$> forget blocks)
    IList marker blocks => line marker <++> hcat (pretty <$> forget blocks)
    Indentation count inline => hcat (pretty <$> forget inline)

Pretty MDDoc where
  prettyPrec _ mdd = vsep (pretty <$> forget mdd)

[localInline] Pretty MDInline where
  prettyPrec _ = \case
    Textual text => line text
    Strong text => enclose "**" "**" (line text)
    Emphasis text => enclose "*" "*" (line text)
    Code text => enclose "`" "`" (line text)
    Link text url => brackets (line text) <+> parens (line url)

[localBlock] Pretty MDBlock where
  prettyPrec _ = \case
    ThematicBreak => line "---"
    Paragraph inline => ""
    Heading level inline => line (replicate (finToNat level) '#') <+> space
    BlankLine count => flush ""
    IListItem blocks => "- "
    IList marker blocks => ""
    Indentation count inline => ""

[localDoc] Pretty MDDoc where
  prettyPrec _ mdd = empty

export
printInline : MDInline -> String
printInline i = trim (Doc.render (Opts 10000) (pretty i))

export
printBlock : MDBlock -> String
printBlock b = trim (Doc.render (Opts 10000) (pretty b))

export
printLocalInline : MDInline -> String
printLocalInline i = trim (Doc.render (Opts 10000) (pretty @{localInline} i))

export
printLocalBlock : MDBlock -> String
printLocalBlock b = trim (Doc.render (Opts 10000) (pretty @{localBlock} b))