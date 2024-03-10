module Main (main) where

import Parser (Display (display), ast, parse)

main :: IO ()
main = interact readParseAndShow

readParseAndShow :: String -> String
readParseAndShow input = case parse ast input of
  Left err -> show err
  Right parsed -> display parsed
