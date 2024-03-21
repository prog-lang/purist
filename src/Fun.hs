{-# LANGUAGE FlexibleInstances #-}

-- | module Fun contains convenient functional primitives used
-- around the codebase.
module Fun ((!>), (|>), Wrap (..)) where

infixl 0 |> -- function application

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

infixl 9 !> -- functional chaining

(!>) :: (a -> b) -> (b -> c) -> a -> c
f !> g = g . f

infixl 6 >*

infixl 6 *<

class (Semigroup a) => Wrap a where
  (>*) :: a -> a -> a
  x >* y = x <> y <> x

  (*<) :: a -> a -> a
  (*<) = flip (>*)

  wrap :: a -> a -> a
  wrap = (>*)

instance Wrap [Char]