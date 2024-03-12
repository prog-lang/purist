{-# LANGUAGE OverloadedStrings #-}

module Pure.Source.Parser (parseModule, ast, expr) where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust)
import Pure.AST (AST (..), Expr (..))
import Text.Parsec
  ( between,
    char,
    digit,
    endBy,
    eof,
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
import Text.Parsec.Number (floating)
import Text.Parsec.String (Parser)

parseModule :: Parsec.SourceName -> String -> Either Parsec.ParseError AST
parseModule = Parsec.parse ast

ast :: Parser AST
ast = endBy def spaces <* eof <&> AST

def :: Parser (String, Expr)
def = do
  n <- name
  _ <- spaces >> string ":=" >> spaces
  e <- expr
  _ <- spaces >> char ';'
  return (n, e)

expr :: Parser Expr
expr = try lambda <|> try iff <|> try app <|> lit <?> "an expression"

lambda :: Parser Expr
lambda = do
  p <- param <?> "a named parameter"
  ex <- expr
  return $ Lam p ex
  where
    param :: Parser String
    param = do
      n <- name
      _ <- spaces >> string "->" >> spaces
      return n

iff :: Parser Expr
iff = do
  _ <- string "if" >> spaces
  b <- condition <?> "a condition"
  _ <- spaces >> string "then" >> spaces
  l <- noIf <?> "a left-hand-side expression"
  _ <- spaces >> string "else" >> spaces
  r <- expr <?> "a right-hand-side expression"
  return $ If b l r

condition :: Parser Expr
condition = try app <|> lit

noIf :: Parser Expr
noIf = try lambda <|> try app <|> lit

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
    <|> Float <$> try float
    <|> Int <$> int

brack :: Parser Expr
brack = between (char '(' >> spaces) (spaces >> char ')') expr

identifier :: Parser String
identifier = do
  parts <- sepBy1 name (char '.') <?> "an identifier"
  return (intercalate "." parts)

name :: Parser String
name = many1 (Parsec.alphaNum <|> char '_' <?> "a name")

str :: Parser String
str = between (char '"') (char '"') (many $ noneOf ['"']) <?> "a string literal"

float :: Parser Double
float = do
  sign <- optionMaybe $ char '-'
  number <- floating
  return $ if isJust sign then -number else number

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