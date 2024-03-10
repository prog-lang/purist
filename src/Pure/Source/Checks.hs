{-# LANGUAGE OverloadedStrings #-}

module Pure.Source.Checks
  ( duplicateDefinitions,
    entrypointPresent,
  )
where

import Data.List (intercalate, nub, (\\))
import Pure.Source.Parser (Ast (..))

-- TYPES

type Error = String

type Result = Either Error Ast

-- CHECKS

duplicateDefinitions :: Ast -> Result
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

entrypointPresent :: Ast -> Result
entrypointPresent ast =
  if elem entrypoint $ names ast
    then Right ast
    else Left err
  where
    err = "entrypoint missing: " ++ entrypoint
    entrypoint = "main"

-- UTILS

names :: Ast -> [String]
names (Ast defs) = map fst defs