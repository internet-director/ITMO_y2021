module HW0.T3
  ( compose
  , contract
  , i
  , k
  , permute
  , s
  ) where

-- S
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

-- K
k :: a -> b -> a
k x y = x

-- I
i :: a -> a
i = s k k

-- B
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

-- W
contract :: (a -> a -> b) -> (a -> b)
contract = s s (s k)

-- C
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k compose) s) (k k)
