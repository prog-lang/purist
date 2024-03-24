{-# LANGUAGE FlexibleInstances #-}

module Pure
  ( Module (..),
    Id,
    moduleNames,
    Definition (..),
    defName,
    Expr (..),
  )
where

import qualified Pure.Sacred as S
import Strings (bracketed, commad, parenthesised, (+-+))

-- TYPES

data Module = Module
  { definitions :: [Definition],
    exports :: [Id]
  }

data Definition = Id := Expr

type Id = String

data Expr
  = Lam Id Expr
  | If Expr Expr Expr
  | App Expr [Expr]
  | List [Expr]
  | Id Id
  | Str String
  | Float Double
  | Int Integer
  deriving (Eq)

-- INSPECT

moduleNames :: Module -> [Id]
moduleNames (Module defs _) = map defName defs

defName :: Definition -> Id
defName (name := _) = name

-- SHOW

embrace :: Expr -> String
embrace i@(Int _) = show i
embrace f@(Float _) = show f
embrace s@(Str _) = show s
embrace i@(Id _) = show i
embrace l@(List _) = show l
embrace ex = parenthesised $ show ex

instance Show Module where
  show (Module defs es) = unlines $ export : map show defs
    where
      export = S.export +-+ parenthesised (commad es) ++ S.str S.semicolon

instance Show Definition where
  show (name := expr) = name +-+ S.walrus +-+ show expr ++ S.str S.semicolon

instance Show Expr where
  show (Int i) = show i
  show (Float f) = show f
  show (Str s) = show s
  show (Id s) = s
  show (List l) = bracketed $ commad (map show l)
  show (App ex exs) = unwords $ map embrace (ex : exs)
  show (If x y z) = S.if_ +-+ show x +-+ S.then_ +-+ show y +-+ S.else_ +-+ show z
  show (Lam p ex) = p +-+ S.arrow +-+ show ex