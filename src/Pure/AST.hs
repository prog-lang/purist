{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Pure.AST
  ( Module (..),
    moduleNames,
    Statement (..),
    defName,
    Visibility (..),
    visibilityFromMaybe,
    Expr (..),
    Display (..),
  )
where

import Fun ((>*))
import qualified Pure.Source.Sacred as S

-- TYPES

newtype Module = Module [Statement]

data Statement = Def Visibility String Expr

data Visibility = Private | Public deriving (Eq)

data Expr
  = Lam String Expr
  | If Expr Expr Expr
  | App Expr [Expr]
  | Id String
  | Str String
  | Float Double
  | Int Int
  deriving (Eq)

-- CONSTRUCT

visibilityFromMaybe :: Maybe a -> Visibility
visibilityFromMaybe (Just _) = Public
visibilityFromMaybe _ = Private

-- INSPECT

moduleNames :: Module -> [String]
moduleNames (Module ss) = map defName ss

defName :: Statement -> String
defName (Def _ nom _) = nom

-- DISPLAY

class Display t where
  wrapped :: Bool -> t -> String

  display :: t -> String
  display = wrapped False

  wrap :: t -> String
  wrap = wrapped True

instance Display Module where
  wrapped :: Bool -> Module -> String
  wrapped _ (Module defs) = unlines $ map display defs

instance Display Statement where
  wrapped :: Bool -> Statement -> String
  wrapped _ (Def vis name expr) = show vis ++ name ++ " " >* S.walrus ++ display expr ++ S.str S.semicolon

instance Display Expr where
  wrapped :: Bool -> Expr -> String
  wrapped _ (Int i) = show i
  wrapped _ (Float f) = show f
  wrapped _ (Str s) = show s
  wrapped _ (Id s) = s
  wrapped False (App ex exs) = unwords $ map wrap (ex : exs)
  wrapped False (If b l r) = S.if_ ++ " " >* display b ++ S.then_ ++ " " >* display l ++ S.else_ ++ " " ++ display r
  wrapped False (Lam p ex) = p ++ " " >* S.arrow ++ display ex
  wrapped True ex = S.str S.lbrace ++ display ex ++ S.str S.rbrace

-- SHOW

instance Show Module where
  show = display

instance Show Visibility where
  show Private = ""
  show Public = S.public ++ " "

instance Show Expr where
  show = display