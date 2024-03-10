{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parse,
    ast,
    decl,
    expr,
    Expr (..),
  )
where

import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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
    (<|>),
  )
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

parse rule = Parsec.parse rule "(source)"

data Expr
  = Lam String Expr
  | App Expr [Expr]
  | Id String
  | Str String
  | Int Int
  deriving (Show, Eq)

ast :: Parser (HashMap String Expr)
ast = endBy decl spaces <* eof <&> HashMap.fromList

decl :: Parser (String, Expr)
decl = do
  n <- name
  _ <- spaces >> string ":=" >> spaces
  e <- expr
  _ <- spaces >> char ';'
  return (n, e)

expr :: Parser Expr
expr = try lambda <|> try app <|> lit

lambda :: Parser Expr
lambda = do
  p <- param
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
  f <- fun
  _ <- spaces
  args <- sepBy1 lit spaces
  return $ App f args

fun :: Parser Expr
fun = try brack <|> Id <$> try identifier <|> Id <$> name

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
  parts <- sepBy1 name $ char '.'
  return $ intercalate "." parts

name :: Parser String
name = do
  l <- many1 $ lower <|> char '_'
  ls <- many $ Parsec.alphaNum <|> char '_'
  return $ l ++ ls

str :: Parser String
str = between (char '"') (char '"') (many $ noneOf ['"'])

int :: Parser Int
int = try zero <|> nonZero
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