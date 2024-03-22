module Main (main) where

import Convert (Into (..))
import Fun ((|>))
import qualified Node
import Node.Transpiler ()
import qualified Pure
import Pure.Checks (duplicateDefinitions)
import Pure.Parser (parseModule)
import Result (unwrap, (<!>))

main :: IO ()
main = interact with

with :: String -> String
with input =
  parseModule "main.pure" input
    <!> show
    >>= duplicateDefinitions
      |> (into :: (Functor f) => f Pure.Module -> f Node.Module)
      |> (into :: (Functor f) => f Node.Module -> f String)
      |> unwrap
