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

renderClassList :: [Char] -> Text -> Text -> Text
renderClassList [c] _ _ = Text.singleton c
renderClassList s l r = l <> Text.pack s <> r

renderJuxta :: OnigurumaIR -> OnigurumaIR -> Text
renderJuxta a b = render a <> render b

renderAlter :: OnigurumaIR -> OnigurumaIR -> Text
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

toRegex :: (Show a) => Parser a -> Text
toRegex = render . toOnigurumaIR
