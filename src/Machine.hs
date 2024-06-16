{-# LANGUAGE OverloadedStrings #-}

module Machine
  ( module Parsec,
    Reply,
    runParser,
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

runParser :: Text -> Parser -> Reply ParserState
runParser s = run (mkState s)

run :: ParserState -> Parser -> Reply ParserState
run ps@(ParserState "") _ = Success ps
run ps Ok = runOk (uncons ps)
run ps (Char c) = runChar (uncons ps) c
run ps (And a b) = runAnd ps a b
run ps (Or a b) = runOr ps a b
run ps (Many a) = runMany ps a

runOk :: Reply (Char, ParserState) -> Reply ParserState
runOk r = do
  (_, ps) <- r
  run ps Ok

runChar :: Reply (Char, ParserState) -> Char -> Reply ParserState
runChar r c = do
  (hd, tl) <- r
  if c == hd
    then Success tl
    else Error

runAnd :: ParserState -> Parser -> Parser -> Reply ParserState
runAnd ps a b = do
  ps' <- run ps a
  run ps' b

runOr :: ParserState -> Parser -> Parser -> Reply ParserState
runOr ps a b = run ps a <|> run ps b

runMany :: ParserState -> Parser -> Reply ParserState
runMany ps a = do
  ps' <- run ps a
  run ps' (Many a)
