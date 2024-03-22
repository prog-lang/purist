module Strings
  ( commad,
    parenthesised,
    braced,
    bracketed,
    (>*),
  )
where

import Data.List (intercalate)

infixl 6 >*

(>*) :: String -> String -> String
wrapper >* target = wrapper ++ target ++ wrapper

commad :: [String] -> String
commad = intercalate ", "

parenthesised :: String -> String
parenthesised a = "(" ++ a ++ ")"

braced :: String -> String
braced a = "{" ++ a ++ "}"

bracketed :: String -> String
bracketed a = "[" ++ a ++ "]"