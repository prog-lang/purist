module Main (main) where

import Data.Either.Extra (fromEither)
import Fun ((!>))
import Pure.Source.Checks (duplicateDefinitions)
import Pure.Source.Parser (Ast (..), Display (display), parse)

main :: IO ()
main = interact readParseAndShow

readParseAndShow :: String -> String
readParseAndShow input =
  case parse "main.pure" input of
    Left err -> show err
    Right parsed -> check parsed

check :: Ast -> String
check = duplicateDefinitions !> fmap display !> fromEither
