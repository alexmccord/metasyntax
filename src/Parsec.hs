module Parsec where

import Prelude

infixr 2 <|>

infixr 3 <&>

-- I think we need a data structure that holds all the possible paths in a language
-- e.g. you can think of them in terms of sets, like union types -> disjunctive parselets.
--
-- Then we need to walk through each paths in the graph, keeping a stack of parselets
-- we've seen so far. We'll then know that we've formed a cycle, which implies recursive parse.
--
-- One can view these parselets as function calls too. The way you loop things in Haskell is by
-- writing a recursive function. The way you loop things in parsers is by calling the same parser.
-- Such an example is `block` -> `[statement, block]`.
--
-- We'll want to come up with a DSL to generate the state machine graph.
-- Then we can go ahead and walk that graph.

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
