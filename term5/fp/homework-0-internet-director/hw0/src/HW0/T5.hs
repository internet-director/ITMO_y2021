module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ v = v

ns :: Nat a -> Nat a
ns f a b = a (f a b)

nplus :: Nat a -> Nat a -> Nat a
nplus a b f v = b f (a f v)

nmult :: Nat a -> Nat a -> Nat a
nmult a b f = b (a f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural v = ns $ nFromNatural $ v - 1

nToNum :: Num a => Nat a -> a
nToNum v = v (+1) 0
