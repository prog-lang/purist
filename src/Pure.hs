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
import Strings
  ( Parens (..),
    list,
    parenthesised,
    tuple,
    (+-+),
    (+\+),
  )
import Type (Type (..))

-- TYPES

data Module = Module
  { definitions :: [Definition],
    exports :: [Id]
  }

data Definition
  = Id := Expr -- main := 42;
  | TypeDef Id [Id] [Type] -- type Maybe a is | Just a | Nothing;

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
  | Bool Bool
  deriving (Eq)

-- INSPECT

moduleNames :: Module -> [Id]
moduleNames (Module defs _) = map defName defs

defName :: Definition -> Id
defName (name := _) = name
defName (TypeDef name _ _) = name

-- SHOW

instance Show Module where
  show (Module defs es) = unlines $ export : map show defs
    where
      export = S.export +-+ tuple es ++ S.str S.semicolon

instance Show Definition where
  show (name := expr) = name +-+ S.walrus +-+ show expr ++ S.str S.semicolon
  show (TypeDef name poly cons) =
    S.type_
      +-+ name
      +-+ unwords poly
      +-+ S.is
      +\+ unlines (map ((S.str S.bar +-+) . show) cons)
      ++ S.str S.semicolon

instance Show Expr where
  show (Bool bool) = show bool
  show (Int int) = show int
  show (Float number) = show number
  show (Str str) = show str
  show (Id ident) = ident
  show (List l) = list (map show l)
  show (App ex exs) = unwords $ map parens (ex : exs)
  show (If x y z) = S.if_ +-+ show x +-+ S.then_ +-+ show y +-+ S.else_ +-+ show z
  show (Lam p ex) = p +-+ S.arrow +-+ show ex

-- PARENS

instance Parens Expr where
  parens i@(Int _) = show i
  parens f@(Float _) = show f
  parens s@(Str _) = show s
  parens i@(Id _) = show i
  parens l@(List _) = show l
  parens ex = parenthesised $ show ex