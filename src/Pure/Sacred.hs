{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Pure.Sacred where

import Data.List (singleton)
import qualified Data.List as List

-- KEYWORDS

isKeyword :: String -> Bool
isKeyword = flip List.elem keywords

keywords :: [String]
keywords = [public, if_, then_, else_]

public :: String
public = "public"

if_ :: String
if_ = "if"

then_ :: String
then_ = "then"

else_ :: String
else_ = "else"

-- SIGNS

operators :: [String]
operators = [walrus, arrow]

walrus :: String
walrus = ":="

arrow :: String
arrow = "->"

semicolon :: Char
semicolon = ';'

lbrace :: Char
lbrace = '('

rbrace :: Char
rbrace = ')'

underscore :: Char
underscore = '_'

dot :: Char
dot = '.'

minus :: Char
minus = '-'

-- UTILS

str :: Char -> String
str = singleton