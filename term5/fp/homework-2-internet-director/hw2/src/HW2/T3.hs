module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Monoid()

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr get mempty
  where get :: Monoid a => Maybe a -> a -> a
        get Nothing v = v
        get (Just x) v = x <> v

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap (\x -> case x of
                      Left a  -> (a, mempty)
                      Right b -> (mempty, b))