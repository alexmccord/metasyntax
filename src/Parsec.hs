module Parsec where

infixr 2 <|>

infixr 3 <&>

data Parser a
  = Ok
  | Char Char
  | And (Parser a) (Parser a)
  | Or (Parser a) (Parser a)
  | Many (Parser a)

instance (Show a) => Show (Parser a) where
  show Ok = "Ok"
  show (Char c) = "(Char " <> show c <> ")"
  show (And a b) = "(& " <> show a <> " " <> show b <> ")"
  show (Or a b) = "(| " <> show a <> " " <> show b <> ")"
  show (Many a) = "(" <> show a <> ")*"

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = Or

(<&>) :: Parser a -> Parser a -> Parser a
(<&>) = And

char :: Char -> Parser a
char = Char

anyChar :: [Char] -> Parser a
anyChar cs = foldl1 (<|>) (fmap char cs)

string :: String -> Parser a
string s = foldl1 (<&>) (fmap char s)

optional :: Parser a -> Parser a
optional a = a <|> Ok

many :: Parser a -> Parser a
many = Many

aAndManyB :: Parser a -> Parser a -> Parser a
aAndManyB a b = a <&> many b
