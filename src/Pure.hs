{-# LANGUAGE FlexibleInstances #-}

module Pure
  ( Module (..),
    moduleNames,
    public,
    Statement (..),
    defName,
    Visibility (..),
    visibilityFromMaybe,
    Expr (..),
  )
where

import qualified Pure.Sacred as S
import Strings (commad, parenthesised, (>*))

-- TYPES

newtype Module = Module [Statement]

data Statement = Def Visibility String Expr

data Visibility = Private | Public deriving (Eq)

data Expr
  = Lam String Expr
  | If Expr Expr Expr
  | App Expr [Expr]
  | List [Expr]
  | Id String
  | Str String
  | Float Double
  | Int Integer
  deriving (Eq)

-- CONSTRUCT

visibilityFromMaybe :: Maybe a -> Visibility
visibilityFromMaybe (Just _) = Public
visibilityFromMaybe _ = Private

-- INSPECT

moduleNames :: Module -> [String]
moduleNames (Module ss) = map defName ss

public :: Module -> [String]
public (Module ss) = map defName $ filter isPublic ss

defName :: Statement -> String
defName (Def _ nom _) = nom

isPublic :: Statement -> Bool
isPublic (Def Private _ _) = False
isPublic (Def Public _ _) = True

-- SHOW

embrace :: Expr -> String
embrace i@(Int _) = show i
embrace f@(Float _) = show f
embrace s@(Str _) = show s
embrace i@(Id _) = show i
embrace l@(List _) = show l
embrace ex = parenthesised $ show ex

instance Show Module where
  show (Module defs) = unlines $ map show defs

instance Show Statement where
  show (Def vis name expr) =
    show vis
      ++ name
      ++ " " >* S.walrus
      ++ show expr
      ++ S.str S.semicolon

instance Show Visibility where
  show Private = ""
  show Public = S.public ++ " "

instance Show Expr where
  show (Int i) = show i
  show (Float f) = show f
  show (Str s) = show s
  show (Id s) = s
  show (List l) = "[" ++ commad (map show l) ++ "]"
  show (App ex exs) = unwords $ map embrace (ex : exs)
  show (If x y z) =
    S.if_
      ++ " " >* embrace x
      ++ S.then_
      ++ " " >* embrace y
      ++ S.else_
      ++ " "
      ++ embrace z
  show (Lam p ex) = p ++ " " >* S.arrow ++ show ex