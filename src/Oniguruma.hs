{-# LANGUAGE OverloadedStrings #-}

module Oniguruma (toRegex) where

-- See the RE spec: https://github.com/kkos/oniguruma/blob/master/doc/RE.
-- We implement a parser purely to validate that the regular expression that
-- we generate are conformant to oniguruma's regular expression engine.
--
-- Oniguruma is the regular expression used in the context of syntax highlighting
-- because TextMate started out as using it, and everyone ended up depending on
-- TextMate's `.tmLanguage`, so by transitivity everyone else also used oniguruma.

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Parsec

data OnigurumaIR
  = Token Text
  | CharClass (Set Char)
  | Concat OnigurumaIR OnigurumaIR
  | Alter OnigurumaIR OnigurumaIR
  | Repeat OnigurumaIR
  | Except OnigurumaIR
  | Optional OnigurumaIR

emptyToken :: OnigurumaIR
emptyToken = Token Text.empty

toOnigurumaIR :: Parser a -> OnigurumaIR
toOnigurumaIR Ok = emptyToken
toOnigurumaIR (Char c) = Token (Text.singleton c)
toOnigurumaIR (And a b) = Concat (toOnigurumaIR a) (toOnigurumaIR b)
toOnigurumaIR (Or a b) = Alter (toOnigurumaIR a) (toOnigurumaIR b)
toOnigurumaIR (Many a) = Repeat (toOnigurumaIR a)

render :: OnigurumaIR -> Text
render (Token t) = t
render (CharClass s) = renderClass s "[" "]"
render (Concat a b) = renderConcat a b
render (Alter a b) = renderAlter a b
render (Repeat a) = renderRepeat a
render (Except a) = renderExcept a
render (Optional a) = renderOptional a

renderClass :: Set Char -> Text -> Text -> Text
renderClass = renderClassList . Set.toList

renderClassList :: [Char] -> Text -> Text -> Text
renderClassList [c] _ _ = Text.singleton c
renderClassList s l r = l <> Text.pack s <> r

renderConcat :: OnigurumaIR -> OnigurumaIR -> Text
renderConcat a b = render a <> render b

renderAlter :: OnigurumaIR -> OnigurumaIR -> Text
renderAlter a b = "(" <> render a <> "|" <> render b <> ")"

renderRepeat :: OnigurumaIR -> Text
renderRepeat (Token t) = "(?:" <> t <> ")" <> "*"
renderRepeat a = render a <> "*"

renderExcept :: OnigurumaIR -> Text
renderExcept (Token t) = "(?!" <> t <> ")"
renderExcept (CharClass s) = renderClass s "[^" "]"
renderExcept (Concat a b) = "(?!" <> renderConcat a b <> ")"
renderExcept (Alter a b) = "(?!" <> renderAlter a b <> ")"
renderExcept (Repeat a) = "(?!" <> renderRepeat a <> ")"
renderExcept (Except a) = render a
renderExcept (Optional a) = render (Except a)

renderOptional :: OnigurumaIR -> Text
renderOptional (Token t) = "(?:" <> t <> ")"
renderOptional (CharClass s) = renderClass s "[" "]"
renderOptional (Concat a b) = "(?:" <> renderConcat a b <> ")"
renderOptional (Alter a b) = "(?:" <> renderAlter a b <> ")"
renderOptional (Repeat a) = "(?:" <> renderRepeat a <> ")"
renderOptional (Except _) = render emptyToken
renderOptional (Optional a) = renderOptional a

toRegex :: (Show a) => Parser a -> Text
toRegex = render . toOnigurumaIR
