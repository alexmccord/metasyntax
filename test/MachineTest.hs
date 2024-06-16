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

integer :: Parser
integer = anyDigit <&> many (anyDigit <|> underscore)
  where
    underscore = char '_'

expr :: Parser
expr = identifier <|> integer

parseOk :: Parser -> Text -> Test
parseOk p t = TestCase $ assert (isLegal p t)

parseFail :: Parser -> Text -> Test
parseFail p t = TestCase $ assert (not (isLegal p t))

identifierTests :: Test
identifierTests =
  TestLabel "identifiers" $
    test
      [ TestLabel "should begin with alpha" (legal "a"),
        TestLabel "should have variable length" (legal "abc"),
        TestLabel "should have digits after first alpha" (legal "a1"),
        TestLabel "should not be empty" (illegal ""),
        TestLabel "should not begin with digit" (illegal "1")
      ]
  where
    legal = parseOk identifier
    illegal = parseFail identifier

integerTests :: Test
integerTests =
  TestLabel "integers" $
    test
      [ TestLabel "should begin with digit" (legal "1"),
        TestLabel "should have variable length" (legal "123"),
        TestLabel "should have underscores after first digit" (legal "1_2"),
        TestLabel "should not have alphas after first digit" (illegal "1a"),
        TestLabel "should not be empty" (illegal ""),
        TestLabel "should not begin with alpha" (illegal "a")
      ]
  where
    legal = parseOk integer
    illegal = parseFail integer

exprTests :: Test
exprTests =
  TestLabel "exprs" $
    test
      [ TestLabel "should parse as identifier" (legal "a"),
        TestLabel "should parse as integer" (legal "1"),
        TestLabel "should not parse as anything" (illegal "1 a")
      ]
  where
    legal = parseOk expr
    illegal = parseFail expr

tests :: Test
tests =
  test
    [ identifierTests,
      integerTests,
      exprTests
    ]
