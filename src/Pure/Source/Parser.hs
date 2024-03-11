{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pure.Source.Parser
  ( Display (..),
    Ast (..),
    Expr (..),
    parse,
    ast,
    expr,
  )
where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust)
import Text.Parsec
  ( between,
    char,
    digit,
    endBy,
    eof,
    lower,
    many,
    many1,
    noneOf,
    oneOf,
    optionMaybe,
    sepBy1,
    spaces,
    string,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

-- PARSE

parse :: Parsec.SourceName -> String -> Either Parsec.ParseError Ast
parse = Parsec.parse ast

-- TYPES

newtype Ast = Ast [(String, Expr)]

data Expr
  = Lam String Expr
  | App Expr [Expr]
  | Id String
  | Str String
  | Int Int
  deriving (Eq)

-- DISPLAY

class Display t where
  wrapped :: Bool -> t -> String

  display :: t -> String
  display = wrapped False

  wrap :: t -> String
  wrap = wrapped True

instance Show Ast where
  show = display

instance Show Expr where
  show = display

instance Display Ast where
  wrapped :: Bool -> Ast -> String
  wrapped _ (Ast defs) = unlines $ map display defs

instance Display (String, Expr) where
  wrapped :: Bool -> (String, Expr) -> String
  wrapped _ (left, ex) = left ++ " := " ++ display ex ++ ";"

instance Display Expr where
  wrapped :: Bool -> Expr -> String
  wrapped _ (Int i) = show i
  wrapped _ (Str s) = show s
  wrapped _ (Id s) = s
  wrapped False (App ex exs) = unwords $ map wrap (ex : exs)
  wrapped False (Lam p ex) = "\\" ++ p ++ " -> " ++ display ex
  wrapped True ex = "(" ++ display ex ++ ")"

-- PARSERS

ast :: Parser Ast
ast = endBy def spaces <* eof <&> Ast

def :: Parser (String, Expr)
def = do
  n <- name
  _ <- spaces >> string ":=" >> spaces
  e <- expr
  _ <- spaces >> char ';'
  return (n, e)

expr :: Parser Expr
expr = try lambda <|> try app <|> lit <?> "an expression"

lambda :: Parser Expr
lambda = do
  p <- param <?> "a named parameter"
  Lam p <$> expr
  where
    param :: Parser String
    param = do
      _ <- char '\\'
      n <- name
      _ <- spaces >> string "->" >> spaces
      return n

app :: Parser Expr
app = do
  f <- fun <?> "a caller function/expression"
  _ <- spaces
  args <- sepBy1 lit spaces <?> "a list of arguments"
  return (App f args)

fun :: Parser Expr
fun =
  try brack
    <|> Id <$> try identifier
    <|> Id <$> name

lit :: Parser Expr
lit =
  try brack
    <|> Id <$> try identifier
    <|> Id <$> try name
    <|> Str <$> try str
    <|> Int <$> int

brack :: Parser Expr
brack = between (char '(' >> spaces) (spaces >> char ')') expr

identifier :: Parser String
identifier = do
  parts <- sepBy1 name (char '.') <?> "an identifier"
  return (intercalate "." parts)

name :: Parser String
name = do
  l <- many1 (lower <|> char '_' <?> err)
  ls <- many (Parsec.alphaNum <|> char '_' <?> err)
  return (l ++ ls)
  where
    err = "a name"

str :: Parser String
str = between (char '"') (char '"') (many $ noneOf ['"']) <?> "a string literal"

int :: Parser Int
int = try zero <|> nonZero <?> "an int literal"
  where
    zero = char '0' >> return 0

    positive = do
      firstDigit <- oneOf ['1' .. '9'] -- Ensures the first digit is not '0'
      restDigits <- many digit -- The rest can be any digit
      return $ read $ firstDigit : restDigits

    nonZero = do
      sign <- optionMaybe $ char '-'
      number <- positive
      return $ if isJust sign then -number else number