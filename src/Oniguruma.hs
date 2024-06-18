{-# LANGUAGE OverloadedStrings #-}

module Oniguruma (toRegex) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Parsec

data OnigurumaIR
  = Token Text
  | CharClass (Set Char)
  | Juxta OnigurumaIR OnigurumaIR
  | Alter OnigurumaIR OnigurumaIR
  | Repeat OnigurumaIR
  | Except OnigurumaIR
  | Optional OnigurumaIR

emptyToken :: OnigurumaIR
emptyToken = Token Text.empty

-- Don't forget about Chomsky's hierarchy!
-- Parsers are cyclic, but regular expressions cannot have cyclic rules.
--
-- As such, we'll need to detect the point where we're about to cycle.
-- When we do that, we'll need to come up with a name for the cycle so that
-- we can add it to the vector of patterns, plus a mapping from the name to the rule.
--
--   num = 0 | succ num
--
-- This is a valid context-free language, but not a valid regular expression.
toOnigurumaIR :: Parser a -> OnigurumaIR
toOnigurumaIR Ok = emptyToken
toOnigurumaIR (Char c) = Token (Text.singleton c)
toOnigurumaIR (And a b) = Juxta (toOnigurumaIR a) (toOnigurumaIR b)
toOnigurumaIR (Or a b) = Alter (toOnigurumaIR a) (toOnigurumaIR b)
toOnigurumaIR (Many a) = Repeat (toOnigurumaIR a)

render :: OnigurumaIR -> Text
render (Token t) = t
render (CharClass s) = renderClass s "[" "]"
render (Juxta a b) = renderJuxta a b
render (Alter a b) = renderAlter a b
render (Repeat a) = renderRepeat a
render (Except a) = renderExcept a
render (Optional a) = renderOptional a

renderClass :: Set Char -> Text -> Text -> Text
renderClass = renderClassList . Set.toList
  where
    renderClassList [c] _ _ = Text.singleton c
    renderClassList s l r = l <> Text.pack s <> r

renderJuxta :: OnigurumaIR -> OnigurumaIR -> Text
renderJuxta a b = render a <> render b

renderAlter :: OnigurumaIR -> OnigurumaIR -> Text
renderAlter a (Token "") = renderOptional a
renderAlter (Token "") b = renderOptional b
renderAlter a b = "(" <> render a <> "|" <> render b <> ")"

renderRepeat :: OnigurumaIR -> Text
renderRepeat (Token t) = "(?:" <> t <> ")" <> "*"
renderRepeat a = render a <> "*"

renderExcept :: OnigurumaIR -> Text
renderExcept (Token t) = "(?!" <> t <> ")"
renderExcept (CharClass s) = renderClass s "[^" "]"
renderExcept (Juxta a b) = "(?!" <> renderJuxta a b <> ")"
renderExcept (Alter a b) = "(?!" <> renderAlter a b <> ")"
renderExcept (Repeat a) = "(?!" <> renderRepeat a <> ")"
renderExcept (Except a) = render a
renderExcept (Optional a) = render (Except a)

renderOptional :: OnigurumaIR -> Text
renderOptional (Token t) = "(?:" <> t <> ")"
renderOptional (CharClass s) = renderClass s "[" "]"
renderOptional (Juxta a b) = "(?:" <> renderJuxta a b <> ")"
renderOptional (Alter a b) = "(?:" <> renderAlter a b <> ")"
renderOptional (Repeat a) = "(?:" <> renderRepeat a <> ")"
renderOptional (Except _) = render emptyToken
renderOptional (Optional a) = renderOptional a

toRegex :: Parser a -> Text
toRegex = render . toOnigurumaIR
