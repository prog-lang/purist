module Main (main) where

import Fun ((|>))
import NodeJS.Transpiler (transpile)
import Pure.Checks (duplicateDefinitions)
import Pure.Source.Parser (parseModule)
import Result (unwrap, (<!>))

main :: IO ()
main = interact with

with :: String -> String
with input =
  show <!> parseModule "main.pure" input
    >>= duplicateDefinitions
    >>= transpile
      |> unwrap
