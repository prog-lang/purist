{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pure.AST (AST (..), Expr (..), Display (..)) where

-- TYPES

newtype AST = AST [(String, Expr)]

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

instance Display AST where
  wrapped :: Bool -> AST -> String
  wrapped _ (AST defs) = unlines $ map display defs

instance Display (String, Expr) where
  wrapped :: Bool -> (String, Expr) -> String
  wrapped _ (left, ex) = left ++ " := " ++ display ex ++ ";"

instance Display Expr where
  wrapped :: Bool -> Expr -> String
  wrapped _ (Int i) = show i
  wrapped _ (Str s) = show s
  wrapped _ (Id s) = s
  wrapped False (App ex exs) = unwords $ map wrap (ex : exs)
  wrapped False (Lam p ex) = p ++ " -> " ++ display ex
  wrapped True ex = "(" ++ display ex ++ ")"

-- SHOW

instance Show AST where
  show = display

instance Show Expr where
  show = display