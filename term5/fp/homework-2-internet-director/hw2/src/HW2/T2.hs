module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn val = foldr sep ([] :| []) where
  sep x (begin :| end)
    | x == val = [] :| (begin : end)
    | otherwise = (x : begin) :| end

joinWith :: a -> NonEmpty [a] -> [a]
joinWith val (begin :| end) =
  begin ++ foldr (\first second -> val : (first ++ second)) [] end
