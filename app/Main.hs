module Main (main) where

import Convert (Into (..))
import Fun ((|>))
import qualified Node
import Node.Transpiler ()
import qualified Pure
import Pure.Checks (duplicateDefinitions)
import Pure.Parser (parseModule)
import Result (Result, unwrap, (<!>))

main :: IO ()
main = interact with

with :: String -> String
with input =
  show
    <!> parseModule "main.pure" input
    |> check
    |> intoJS
    |> intoCode
    |> unwrap

check :: Result String Pure.Module -> Result String Pure.Module
check = (>>= duplicateDefinitions)

intoJS :: Result String Pure.Module -> Result String Node.Module
intoJS = fmap into

intoCode :: Result String Node.Module -> Result String String
intoCode = fmap show
