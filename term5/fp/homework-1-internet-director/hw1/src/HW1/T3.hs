module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = Int

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch sz _ _ _) = sz

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ a _ b) = 1 + max (tdepth a) (tdepth b)

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ a v b)
    | x < v = tmember x a
    | x > v = tmember x b
    | otherwise = True

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = Branch 1 Leaf x Leaf
tinsert x (Branch sz a v b)
    | x < v = Branch (sz + 1) (tinsert x a) v b
    | x > v = Branch (sz + 1) a v (tinsert x b)
    | otherwise = Branch sz a v b

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
