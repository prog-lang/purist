module Main (main) where

import Convert (Into (..))
import Fun ((!>), (|>))
import qualified Node
import Node.Transpiler ()
import Pure.Checks (duplicateDefinitions)
import Pure.Parser (parseModule)
import Result (Result (..), (<!>))
import System.IO (hPutStr, stderr)

main :: IO ()
main = getContents >>= transpile !> printOut

printOut :: (Show a1, Show a2) => Result a1 a2 -> IO ()
printOut (Ok ok) = print ok
printOut (Err err) = hPutStr stderr $ show err

transpile :: String -> Result String Node.Module
transpile input =
  parseModule "main.pure" input
    <!> show
    >>= duplicateDefinitions
      |> into
