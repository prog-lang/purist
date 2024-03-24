module Strings
  ( commad,
    parenthesised,
    braced,
    bracketed,
    array,
    tuple,
    list,
    (>*),
    (+-+),
  )
where

import Data.List (intercalate)

infixl 6 >*

(>*) :: String -> String -> String
wrapper >* target = wrapper ++ target ++ wrapper

infixr 5 +-+

(+-+) :: String -> String -> String
x +-+ y = x ++ " " ++ y

commad :: [String] -> String
commad = intercalate ", "

parenthesised :: String -> String
parenthesised a = "(" ++ a ++ ")"

braced :: String -> String
braced a = "{" ++ a ++ "}"

bracketed :: String -> String
bracketed a = "[" ++ a ++ "]"

tuple :: [String] -> String
tuple = parenthesised . commad

array :: [String] -> String
array = braced . commad

list :: [String] -> String
list = bracketed . commad