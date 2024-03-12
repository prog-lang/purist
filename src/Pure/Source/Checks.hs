{-# LANGUAGE OverloadedStrings #-}

module Pure.Source.Checks (duplicateDefinitions, entrypointPresent) where

import Data.List (intercalate, nub, (\\))
import Pure.AST (AST (..))

-- TYPES

type Error = String

type Result = Either Error AST

-- CHECKS

duplicateDefinitions :: AST -> Result
duplicateDefinitions ast =
  if null duplicates
    then Right ast
    else Left err
  where
    err = prefix ++ intercalate ", " duplicates
    prefix = "module contains duplicate definitions: "
    duplicates = ns \\ unique
    unique = nub ns
    ns = names ast

entrypointPresent :: AST -> Result
entrypointPresent ast =
  if elem entrypoint $ names ast
    then Right ast
    else Left err
  where
    err = "entrypoint missing: " ++ entrypoint
    entrypoint = "main"

-- UTILS

names :: AST -> [String]
names (AST defs) = map fst defs