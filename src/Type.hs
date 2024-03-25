module Type (Type (..)) where

import Strings (Parens (..), parenthesised, (+-+))

type Name = String

data Type = Type Name [Type]

instance Parens Type where
  parens this@(Type _ []) = show this
  parens t = parenthesised $ show t

instance Show Type where
  show (Type tag []) = tag
  show (Type tag args) = tag +-+ unwords (map parens args)