{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language
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
identifier = Parsec.name "identifier" $ aAndManyB anyAlpha anyAlnum

integer :: MyParser
integer = Parsec.name "integer" $ aAndManyB anyDigit (anyDigit <|> underscore)
  where
    underscore = char '_'

expr :: MyParser
expr = identifier <|> integer

myLang :: TmLanguage
myLang = makeTmLanguage "Lang" "source.lang" "lang" $ do
  return ()

main :: IO ()
main = print myLang
