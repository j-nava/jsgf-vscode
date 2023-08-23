module Text.JSGF.Print

import Data.String
import Text.PrettyPrint.Bernardy

import Text.JSGF.Types

-- Pretty MDInline where
--   prettyPrec _ = \case
--     Textual text => line text
--     Strong text => enclose "**" "**" (line text)
--     Emphasis text => enclose "*" "*" (line text)
--     Code text => enclose "`" "`" (line text)
--     Link text url => brackets (line text) <+> parens (line url)

-- Pretty MDBlock where
--   prettyPrec _ = \case
--     ThematicBreak => line "---"
--     Paragraph inline => hcat (pretty <$> forget inline)
--     Heading level inline => line (replicate (finToNat level) '#') <++> hcat (pretty <$> forget inline)
--     BlankLine count => flush ""
--     IListItem blocks => hcat (pretty <$> forget blocks)
--     IList marker blocks => line marker <++> hcat (pretty <$> forget blocks)
--     Indentation count inline => hcat (pretty <$> forget inline)

-- Pretty Block where
--   prettyPrec _ = \case
--     BSelfIdent selfIdent => line ""

-- Pretty Doc where
--   -- prettyPrec _ (MkDoc si) = vsep (pretty <$> forget jsgf)
--   prettyPrec _ (MkDoc si) = pretty si
