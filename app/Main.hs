{-# LANGUAGE OverloadedStrings #-}

module Main where

import Machine
import Parsec

anyAlpha :: Parser
anyAlpha = underscore <|> lowercase <|> uppercase
  where
    underscore = char '_'
    lowercase = anyChar ['a' .. 'z']
    uppercase = anyChar ['A' .. 'Z']

anyDigit :: Parser
anyDigit = anyChar ['0' .. '9']

anyAlnum :: Parser
anyAlnum = anyAlpha <|> anyDigit

identifier :: Parser
identifier = aAndManyB anyAlpha anyAlnum

main :: IO ()
main = print $ runParser "aaaa1" identifier
