{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Node.Transpiler where

import Convert (Into (..))
import qualified Node
import Pure (Definition ((:=)))
import qualified Pure

type Error = String

instance Into Node.Module String where
  into = show

instance Into Pure.Module Node.Module where
  into modul =
    Node.Module $
      map into (Pure.definitions modul)
        ++ [Node.Exports $ Pure.exports modul]

instance Into Pure.Definition Node.Statement where
  into (name := (Pure.Lam param expr)) = Node.Function name [param] [Node.Return $ into expr]
  into (name := expr) = Node.Const name $ into expr

instance Into Pure.Expr Node.Expr where
  into (Pure.Lam param body) = Node.Lam [param] [Node.Return $ into body]
  into (Pure.If b l r) = Node.Ternary (into b) (into l) (into r)
  into (Pure.App ex exs) = foldl call (into ex) exs
    where
      call f arg = Node.Call f [into arg]
  into (Pure.List list) = Node.Array $ map into list
  into (Pure.Id ident) = Node.Id ident
  into (Pure.Str str) = Node.Str str
  into (Pure.Float n) = Node.Float n
  into (Pure.Int i) = Node.Int i
