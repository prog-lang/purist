{-# LANGUAGE FlexibleInstances #-}

module Pure.Module
  ( Module (..),
    moduleNames,
    Statement (..),
    defName,
    Visibility (..),
    visibilityFromMaybe,
    Expr (..),
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

isIf :: Expr -> Bool
isIf (If {}) = True
isIf _ = False

-- SHOW

embrace :: Expr -> String
embrace (Int i) = show i
embrace (Float f) = show f
embrace (Str s) = show s
embrace (Id s) = s
embrace ex = S.str S.lbrace ++ show ex ++ S.str S.rbrace

embraceIf :: (Expr -> Bool) -> Expr -> String
embraceIf check ex = if check ex then embrace ex else show ex

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
  show (App ex exs) = unwords $ map embrace (ex : exs)
  show (If b l r) =
    S.if_
      ++ " " >* show b
      ++ S.then_
      ++ " " >* embraceIf isIf l
      ++ S.else_
      ++ " "
      ++ show r
  show (Lam p ex) = p ++ " " >* S.arrow ++ show ex