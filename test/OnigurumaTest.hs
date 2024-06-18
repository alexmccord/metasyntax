{-# LANGUAGE OverloadedStrings #-}

module OnigurumaTest (tests) where

import Data.Text
import qualified Oniguruma
import Parsec
import Test.HUnit

keywordLet :: Parser ()
keywordLet = string "let"

keywordIn :: Parser ()
keywordIn = string "in"

keyword :: Parser ()
keyword = keywordLet <|> keywordIn

renderEq :: String -> Parser a -> Text -> Test
renderEq c p t = TestCase $ assertEqual c (Oniguruma.toRegex p) t

keywordTests :: Test
keywordTests =
  TestLabel "keywords" $
    test
      [ renderEq "should render reasonable regex let" keywordLet "let",
        renderEq "should render reasonable regex in" keywordIn "in",
        renderEq "should render reasonable regex for all keywords" keyword "(let|in)"
      ]

tests :: Test
tests = test [keywordTests]
