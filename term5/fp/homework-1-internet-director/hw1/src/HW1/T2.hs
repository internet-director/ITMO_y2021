module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N deriving Show

nplus :: N -> N -> N
nplus Z b = b
nplus (S a) b = S (nplus a b)

nmult :: N -> N -> N
nmult Z _ = Z
nmult (S a) b = nplus b (nmult a b)

nsub :: N -> N -> Maybe N
nsub a Z = Just a
nsub Z _ = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S a) (S b) = ncmp a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural a = S (nFromNatural (a - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S a) = (nToNum (a)) + 1

nEven :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S a)) = nEven a

nOdd :: N -> Bool
nOdd a = not (nEven a)

ndiv :: N -> N -> N
ndiv Z _ = Z
ndiv a Z = error "Division by zero"
ndiv a b | ncmp a b == LT = Z
         | ncmp a b == EQ = (S Z)
         | otherwise = case nsub a b of
      Just res -> S (ndiv res b)
      Nothing -> Z

nmod :: N -> N -> N
nmod Z _ = Z
nmod a b = case (nsub a (nmult b (ndiv a b))) of
        Just res -> res
        Nothing -> Z

