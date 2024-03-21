module Main (main) where

import Convert (Into (..))
import Fun ((|>))
import qualified NodeJS.CommonJS as JS
import NodeJS.Transpiler ()
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

intoJS :: Result String Pure.Module -> Result String JS.Module
intoJS = fmap into

intoCode :: Result String JS.Module -> Result String String
intoCode = fmap show
