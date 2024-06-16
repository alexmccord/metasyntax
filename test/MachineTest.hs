{-# LANGUAGE OverloadedStrings #-}

module MachineTest (tests) where

import Data.Text
import Machine
import Parsec
import Test.HUnit

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

expr :: Parser
expr = identifier

parseOk :: Text -> Test
parseOk t = TestCase $ assert (isLegal expr t)

parseFail :: Text -> Test
parseFail t = TestCase $ assert (not (isLegal expr t))

identifierTests :: Test
identifierTests =
  TestLabel "identifiers" $
    test
      [ TestLabel "should begin with alpha" (parseOk "a"),
        TestLabel "should have variable length" (parseOk "aaa"),
        TestLabel "should not be empty" (parseFail ""),
        TestLabel "should not begin with numbers" (parseFail "1")
      ]

tests :: Test
tests = test [identifierTests]
