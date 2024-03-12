module Pure.Source.Checks (duplicateDefinitions, entrypointPresent) where

import Data.List (intercalate, nub, (\\))
import Pure.AST (Module (..), moduleNames)

-- TYPES

type Error = String

type Result = Either Error Module

-- CHECKS

duplicateDefinitions :: Module -> Result
duplicateDefinitions ast =
  if null duplicates
    then Right ast
    else Left err
  where
    err = prefix ++ intercalate ", " duplicates
    prefix = "module contains duplicate definitions: "
    duplicates = ns \\ unique
    unique = nub ns
    ns = moduleNames ast

entrypointPresent :: Module -> Result
entrypointPresent ast =
  if elem entrypoint $ moduleNames ast
    then Right ast
    else Left err
  where
    err = "entrypoint missing: " ++ entrypoint
    entrypoint = "main"
