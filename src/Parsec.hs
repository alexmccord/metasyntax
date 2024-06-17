module Parsec where

import Prelude

infixr 2 <|>

infixr 3 <&>

data (Show a) => Parser a
  = Ok
  | Char Char
  | And (Parser a) (Parser a)
  | Or (Parser a) (Parser a)
  | Many (Parser a)
  | Name a (Parser a)

instance (Show a) => Show (Parser a) where
  show Ok = "Ok"
  show (Char c) = "(Char " <> show c <> ")"
  show (And a b) = "(& " <> show a <> " " <> show b <> ")"
  show (Or a b) = "(| " <> show a <> " " <> show b <> ")"
  show (Many a) = "(" <> show a <> ")*"
  show (Name n _) = show n

(<|>) :: (Show a) => Parser a -> Parser a -> Parser a
(<|>) = Or

(<&>) :: (Show a) => Parser a -> Parser a -> Parser a
(<&>) = And

char :: (Show a) => Char -> Parser a
char = Char

anyChar :: (Show a) => [Char] -> Parser a
anyChar cs = foldl1 (<|>) (fmap char cs)

optional :: (Show a) => Parser a -> Parser a
optional a = a <|> Ok

many :: (Show a) => Parser a -> Parser a
many = Many

aAndManyB :: (Show a) => Parser a -> Parser a -> Parser a
aAndManyB a b = a <&> many b

name :: (Show a) => a -> Parser a -> Parser a
name = Name
