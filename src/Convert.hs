{-# LANGUAGE MultiParamTypeClasses #-}

module Convert (Into (..), TryInto (..)) where

import Result (Result)

class Into a b where
  into :: a -> b

class TryInto a err ok where
  tryInto :: a -> Result err ok