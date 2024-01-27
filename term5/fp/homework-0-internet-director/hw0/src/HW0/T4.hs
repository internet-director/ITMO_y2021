module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix $ \v v1 ->
    case v1 of
        []     -> []
        y : ys -> f y : v ys

fib :: Natural -> Natural
fib n
    | n < 1     = 0
    | otherwise = fibFast 1 1 (n - 1)

fibFast :: Natural -> Natural -> Natural -> Natural
fibFast _ f2 0  = f2
fibFast _ f2 1  = f2
fibFast f1 f2 n = fibFast f2 (f1 + f2) (n - 1)

fac :: Natural -> Natural
fac n = if n < 1 then 1 else fac' n

fac' :: Natural -> Natural
fac' a = case a of
    1 -> 1
    n -> n * fac (a - 1)
