module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ y Leaf = y
tfoldr f y (Branch _ l v r) = tfoldr f (f v (tfoldr f y r)) l

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
