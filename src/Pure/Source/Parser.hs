{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Pure.Source.Parser (parseModule, ast, expr) where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust)
import Pure.AST
  ( Expr (..),
    Module (..),
    Statement (..),
    visibilityFromMaybe,
  )
import qualified Pure.Source.Sacred as S
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

parseModule :: Parsec.SourceName -> String -> Either Parsec.ParseError Module
parseModule = Parsec.parse ast

ast :: Parser Module
ast = endBy def spaces <* eof <&> Module

def :: Parser Statement
def = do
  vis <- optionMaybe $ string S.public
  _ <- spaces
  nom <- name
  _ <- spaces >> string S.walrus >> spaces
  ex <- expr
  _ <- spaces >> char S.semicolon
  return $ Def (visibilityFromMaybe vis) nom ex

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
      _ <- spaces >> string S.arrow >> spaces
      return n

iff :: Parser Expr
iff = do
  _ <- string S.if_ >> spaces
  b <- condition <?> "a condition"
  _ <- spaces >> string S.then_ >> spaces
  l <- noIf <?> "a left-hand-side expression"
  _ <- spaces >> string S.else_ >> spaces
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
  return $ App f args

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
brack = between (char S.lbrace >> spaces) (spaces >> char S.rbrace) expr

ident :: Parser String
ident = do
  parts <- sepBy1 name (char S.dot) <?> "an identifier"
  return (intercalate (S.str S.dot) parts)

name :: Parser String
name = many1 (Parsec.alphaNum <|> char S.underscore <?> "a name")

str :: Parser String
str = stringLiteral $ makeTokenParser haskellDef

float :: Parser Double
float = do
  sign <- optionMaybe $ char S.minus
  number <- floating
  return $ if isJust sign then -number else number
