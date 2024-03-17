module NodeJS.Transpiler (transpile) where

import Pure.Module (Expr (..), Module (..), Statement (..), Visibility (..))
import Result

type Error = String

transpile :: Module -> Result Error String
transpile modul = Ok "console.log('ok')"
