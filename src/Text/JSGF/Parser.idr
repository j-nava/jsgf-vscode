module Text.JSGF.Parser

import Data.String
import Text.Parser
import Text.JSGF.Token
import Text.JSGF.Types


-- textual : Grammar state MDToken True String
-- textual = do
--   text <- forget <$> some (choice [match MDText, match MDDash, match MDHash, match MDSpace, match MDLParens, match MDRParens])
--   pure (foldl ((++)) "" text)


-- count' : (qty : Quantity) -> (p : Grammar state MDToken True a) -> {auto 0 _ : IsSucc (qty.min)} -> Grammar state MDToken True (List a)
-- count' q@(Qty (S _) _) p @{ItIsSucc} = count q p

-- withIndent : (indentLevel : Nat) -> (p : Grammar state MDToken True a) -> 
--               {auto 0 prf : IsSucc indentLevel} -> 
--               Grammar state MDToken True a
-- withIndent (S indentLevel) p = count' (exactly (S indentLevel)) p *> p

-- inlineCode : Grammar state MDToken True MDInline
-- inlineCode = pure (Code !(between (match MDBacktick) (match MDBacktick) textual))

-- inlineStrong : Grammar state MDToken True MDInline
-- inlineStrong =
--   let sep1 : Grammar _ _ _ _ = match MDAsterisk *> match MDAsterisk 
--       sep2 : Grammar _ _ _ _ = match MDUnderscore *> match MDUnderscore
--   in pure $ Strong !(between sep1 sep1 textual <|> between sep2 sep2 textual)

-- inlineEmphasis : Grammar state MDToken True MDInline
-- inlineEmphasis = 
--   let sep1 = match MDAsterisk
--       sep2 = match MDUnderscore
--   in pure $ Emphasis !(between sep1 sep1 textual <|> between sep2 sep2 textual)

-- inlineLink : Grammar state MDToken True MDInline
-- inlineLink = do
--   text <- between (match MDLBracket) (match MDRBracket) textual
--   link <- between (match MDLParens) (match MDRParens) textual
--   pure (Link text link)

-- inlineTextual : Grammar state MDToken True MDInline
-- inlineTextual = pure (Textual !textual)
  
-- inlineTextfallback : Grammar state MDToken True MDInline
-- inlineTextfallback = do
--   text <- forget <$> some (
--           match MDBacktick <|> match MDAsterisk <|> match MDUnderscore <|> match MDSpace <|>
--           match MDLBracket <|> match MDRBracket)
--   pure (Textual (foldl ((++)) "" text))

-- inline : Grammar state MDToken True MDInline
-- inline = inlineLink <|> inlineStrong <|> inlineEmphasis <|> inlineCode <|> inlineTextual <|> inlineTextfallback

-- inlines : Grammar state MDToken True (List1 MDInline)
-- inlines = some inline <* match MDLineBreak

-- -- LEAF BLOCKS
-- heading : Grammar state MDToken True (MDBlock { blockItem = AnyBlockItem })
-- heading = do
--   level <-  some (match MDHash) 
--             <&> length
--             <&> (\l => natToFin l 6)
--             >>= maybe (fail "Heading level too deep") pure
--   _ <- match MDSpace
--   pure (Heading level !inlines)

-- paragraph : Grammar state MDToken True (MDBlock { blockItem = AnyBlockItem })
-- paragraph = pure (Paragraph !inlines)

-- blankLine : Grammar state MDToken True (MDBlock { blockItem = AnyBlockItem })
-- blankLine = do
--   count <- some (match MDLineBreak)
--            <&> length
--   pure (BlankLine count)

-- indentation : Grammar state MDToken True (MDBlock { blockItem = AnyBlockItem })
-- indentation = pure (Indentation !(length <$> some (match MDTab)) !inlines)

-- leafBlock : Grammar state MDToken True (MDBlock { blockItem = AnyBlockItem })
-- leafBlock = indentation <|> heading <|> blankLine <|> paragraph

-- -- ------------------------------------------------------------
-- -- CONTAINER BLOCKS
-- ilistItem : MDToken -> Grammar state MDToken True (MDBlock { blockItem = ListItemBlockItem })
-- ilistItem marker = do
--   _ <- match marker.kind
--   indentLevel <- (length) <$> some (match MDSpace)
--   mainBlock <- leafBlock
--   blocks <- many (withIndent (S indentLevel) leafBlock) -- "S" indent to account for marker
--   pure (IListItem (mainBlock ::: blocks))

-- ilist : Grammar state MDToken True (MDBlock { blockItem = AnyBlockItem })
-- ilist = 
--   let listMarkers = [MDDash, MDAsterisk]
--   in do sep <- peek
--         case (isJust $ find ((==) sep.kind) listMarkers) of
--           True => do
--             blocks <- some (ilistItem sep)
--             pure (IList sep.text blocks)
--           False => fail "Not a list"

-- -- ------------------------------------------------------------

-- block : Grammar state MDToken True (MDBlock { blockItem = AnyBlockItem })
-- block = ilist <|> leafBlock

selfIdent : Grammar state JSGFToken True SelfIdent
selfIdent = do
  gg <- match JSGFSpace
  pure (MkSelfIdent "" Nothing Nothing)

block : Grammar state JSGFToken True Block
block = do
  gg <- match JSGFSpace
  pure (BSelfIdent !selfIdent)

doc : Grammar state JSGFToken True Doc
doc = pure !(some block)

export
jsgfParse : List (WithBounds JSGFToken) -> Either (List1 (ParsingError JSGFToken)) Doc
jsgfParse tokList = 
  case parse doc tokList of
    Right (r, Nil) => Right r
    -- Right (r, l)   => Right ((Paragraph (Textual (joinBy "" ((\e => show e.val.kind) <$> l)) ::: [])) ::: forget r)
    Right (r, l)   => Right r
    -- Right _        => Left $ (Error "Document not fully parsed" Nothing) ::: Nil
    Left err       => Left err
