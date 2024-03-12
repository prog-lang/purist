-- | module Fun contains convenient functional primitives used
-- around the codebase.
module Fun ((!>), (|>)) where

infixl 2 |> -- function application

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

infixl 9 !> -- functional chaining

(!>) :: (a -> b) -> (b -> c) -> a -> c
f !> g = g . f
