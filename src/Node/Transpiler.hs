{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Node.Transpiler where

import Convert (Into (..))
import qualified Node
import qualified Pure

type Error = String

instance Into Pure.Module Node.Module where
  into :: Pure.Module -> Node.Module
  into modul@(Pure.Module ss) = Node.Module $ map into ss ++ [exports]
    where
      exports = Node.Exports $ Pure.public modul

instance Into Pure.Statement Node.Statement where
  into :: Pure.Statement -> Node.Statement
  into (Pure.Def _ name (Pure.Lam param ex)) = Node.Function name [param] body
    where
      body = [Node.Return $ into ex]
  into (Pure.Def _ ident expr) = Node.Const ident $ into expr

instance Into Pure.Expr Node.Expr where
  into :: Pure.Expr -> Node.Expr
  into (Pure.Lam param body) = Node.Lam [param] [Node.Return $ into body]
  into (Pure.If b l r) = Node.Ternary (into b) (into l) (into r)
  into (Pure.App ex exs) = foldl call (into ex) exs
    where
      call f arg = Node.Call f [into arg]
  into (Pure.Id ident) = Node.Id ident
  into (Pure.Str str) = Node.Str str
  into (Pure.Float n) = Node.Float n
  into (Pure.Int i) = Node.Int i
