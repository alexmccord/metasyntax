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

data Parser
  = Ok
  | Char Char
  | And Parser Parser
  | Or Parser Parser
  | Many Parser

(<|>) :: Parser -> Parser -> Parser
(<|>) = Or

(<&>) :: Parser -> Parser -> Parser
(<&>) = And

char :: Char -> Parser
char = Char

anyChar :: [Char] -> Parser
anyChar cs = foldl1 (<|>) (fmap char cs)

optional :: Parser -> Parser
optional a = a <|> Ok

many :: Parser -> Parser
many = Many

aAndManyB :: Parser -> Parser -> Parser
aAndManyB a b = a <&> many b
