{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module NodeJS.CommonJS (Statement (..), Expr (..)) where

import Data.List (intercalate)
import Fun ((>*))
import qualified NodeJS.Sacred as S

-- TYPES

-- | Any valid CommonJS identifier
type Id = String

data Statement
  = Const Id Expr
  | Var Id Expr
  | Let Id Expr
  | Assign Id Expr
  | Return Expr
  | Exports [Id]
  deriving (Eq)

data Expr
  = Int Int
  | Float Double
  | Str String
  | Id Id
  | New Id [Expr]
  | Ternary Expr Expr Expr
  | Function [Id] [Statement]
  | Call Expr [Expr]
  deriving (Eq)

-- INSPECT

isTernary :: Expr -> Bool
isTernary (Ternary {}) = True
isTernary _ = False

-- SHOW

embrace :: Expr -> String
embrace (Int i) = show i
embrace (Float f) = show f
embrace (Str s) = show s
embrace (Id s) = s
embrace ex = braced $ show ex

embraceIf :: (Expr -> Bool) -> Expr -> String
embraceIf check ex = if check ex then embrace ex else show ex

commad :: [String] -> String
commad = intercalate $ S.comma ++ " "

braced :: String -> String
braced a = S.lbrace ++ a ++ S.rbrace

bracketed :: String -> String
bracketed a = S.lbracket ++ a ++ S.rbracket

instance Show Statement where
  show :: Statement -> String
  show (Const ident ex) = S.const ++ " " ++ ident ++ " " >* S.assign ++ show ex ++ S.semicolon
  show (Var ident ex) = S.var ++ " " ++ ident ++ " " >* S.assign ++ show ex ++ S.semicolon
  show (Let ident ex) = S.let_ ++ " " ++ ident ++ " " >* S.assign ++ show ex ++ S.semicolon
  show (Assign ident ex) = ident ++ " " >* S.assign ++ show ex ++ S.semicolon
  show (Return ex) = S.return ++ " " ++ show ex ++ S.semicolon
  show (Exports ids) = S.exports ++ " " >* S.assign ++ bracketed (commad ids)

instance Show Expr where
  show :: Expr -> String
  show (Int i) = show i
  show (Float n) = show n
  show (Str s) = show s
  show (Id ident) = ident
  show (New ident exs) = S.new ++ ident ++ braced (commad $ map show exs)
  show (Ternary cond left right) = cond_ ++ question ++ then_ ++ colon ++ else_
    where
      cond_ = show cond
      then_ = embraceIf isTernary left
      else_ = embraceIf isTernary right
      question = " " >* S.question
      colon = " " >* S.colon
  show (Function ids ss) = S.function ++ params ++ " " ++ body
    where
      params = braced $ commad ids
      body = bracketed $ show ss
  show (Call ex exs) = unwords $ map embrace (ex : exs)