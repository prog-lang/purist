{-# LANGUAGE OverloadedStrings #-}

module Pure.Source.Parser (parseModule, ast, expr) where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust)
import Pure.AST (AST (..), Expr (..))
import Text.Parsec
  ( between,
    char,
    endBy,
    eof,
    many1,
    optionMaybe,
    sepBy1,
    spaces,
    string,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Parsec as Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Number (floating, int)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (stringLiteral), makeTokenParser)

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
    <|> Id <$> try ident
    <|> Id <$> name

lit :: Parser Expr
lit =
  try brack
    <|> Id <$> try ident
    <|> Id <$> try name
    <|> Str <$> try str
    <|> Float <$> try float
    <|> Int <$> int

brack :: Parser Expr
brack = between (char '(' >> spaces) (spaces >> char ')') expr

ident :: Parser String
ident = do
  parts <- sepBy1 name (char '.') <?> "an identifier"
  return (intercalate "." parts)

name :: Parser String
name = many1 (Parsec.alphaNum <|> char '_' <?> "a name")

str :: Parser String
str = stringLiteral $ makeTokenParser haskellDef

float :: Parser Double
float = do
  sign <- optionMaybe $ char '-'
  number <- floating
  return $ if isJust sign then -number else number
