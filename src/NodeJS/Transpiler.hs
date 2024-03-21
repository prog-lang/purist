{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NodeJS.Transpiler where

import Convert (Into (..))
import qualified NodeJS.CommonJS as JS
import qualified Pure

type Error = String

instance Into Pure.Module JS.Module where
  into :: Pure.Module -> JS.Module
  into modul@(Pure.Module ss) = JS.Module $ map into ss ++ [exports]
    where
      exports = JS.Exports $ Pure.public modul

instance Into Pure.Statement JS.Statement where
  into :: Pure.Statement -> JS.Statement
  into (Pure.Def _ name (Pure.Lam param ex)) = JS.Function name [param] body
    where
      body = [JS.Return $ into ex]
  into (Pure.Def _ ident expr) = JS.Const ident $ into expr

instance Into Pure.Expr JS.Expr where
  into :: Pure.Expr -> JS.Expr
  into (Pure.Lam param body) = JS.Lam [param] [JS.Return $ into body]
  into (Pure.If b l r) = JS.Ternary (into b) (into l) (into r)
  into (Pure.App ex exs) = foldl call (into ex) exs
    where
      call f arg = JS.Call f [into arg]
  into (Pure.Id ident) = JS.Id ident
  into (Pure.Str str) = JS.Str str
  into (Pure.Float n) = JS.Float n
  into (Pure.Int i) = JS.Int i
