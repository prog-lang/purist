{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Pure.Source.Sacred where

import Data.List (singleton)

-- KEYWORDS

public :: String
public = "public"

if_ :: String
if_ = "if"

then_ :: String
then_ = "then"

else_ :: String
else_ = "else"

-- SIGNS

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