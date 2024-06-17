module Main where

import Parsec

type MyParser = Parser String

anyAlpha :: MyParser
anyAlpha = underscore <|> lowercase <|> uppercase
  where
    underscore = char '_'
    lowercase = anyChar ['a' .. 'z']
    uppercase = anyChar ['A' .. 'Z']

anyDigit :: MyParser
anyDigit = anyChar ['0' .. '9']

anyAlnum :: MyParser
anyAlnum = anyAlpha <|> anyDigit

identifier :: MyParser
identifier = name "identifier" $ aAndManyB anyAlpha anyAlnum

integer :: MyParser
integer = name "integer" $ anyDigit <&> many (anyDigit <|> underscore)
  where
    underscore = char '_'

expr :: MyParser
expr = identifier <|> integer

main :: IO ()
main = print expr
