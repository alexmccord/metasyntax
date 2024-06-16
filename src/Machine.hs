{-# LANGUAGE OverloadedStrings #-}

module Machine
  ( module Parsec,
    Reply (..),
    isLegal,
  )
where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Parsec hiding ((<|>))

data Reply a
  = Success a
  | Error
  deriving (Show)

instance Functor Reply where
  fmap f (Success a) = Success (f a)
  fmap _ Error = Error

instance Applicative Reply where
  pure = Success

  Success f <*> Success x = Success (f x)
  Error <*> _ = Error
  _ <*> Error = Error

instance Monad Reply where
  Success x >>= f = f x
  Error >>= _ = Error

instance Alternative Reply where
  empty = Error
  Success a <|> _ = Success a
  Error <|> r = r

newtype ParserState = ParserState Text
  deriving (Show)

mkState :: Text -> ParserState
mkState = ParserState

uncons :: ParserState -> Reply (Char, ParserState)
uncons (ParserState t) = case Text.uncons t of
  Just (hd, tl) -> Success (hd, ParserState tl)
  Nothing -> Error

isLegal :: Parser -> Text -> Bool
isLegal p t = case runParser p t of
  Success (ParserState "") -> True
  Success _ -> error "Reply was successful but ParserState was non-empty"
  Error -> False

runParser :: Parser -> Text -> Reply ParserState
runParser p s = run p (mkState s)

run :: Parser -> ParserState -> Reply ParserState
run Ok ps = Success ps
run (Char c) ps = runChar (uncons ps) c
run (And a b) ps = runAnd a b ps
run (Or a b) ps = runOr a b ps
run (Many a) ps = runMany a ps

runChar :: Reply (Char, ParserState) -> Char -> Reply ParserState
runChar r c = do
  (hd, tl) <- r
  if c == hd
    then Success tl
    else Error

runAnd :: Parser -> Parser -> ParserState -> Reply ParserState
runAnd a b ps = do
  ps' <- run a ps
  run b ps'

runOr :: Parser -> Parser -> ParserState -> Reply ParserState
runOr a b ps = run a ps <|> run b ps

runMany :: Parser -> ParserState -> Reply ParserState
runMany _ ps@(ParserState "") = Success ps
runMany a ps = do
  ps' <- run a ps
  run (Many a) ps'
