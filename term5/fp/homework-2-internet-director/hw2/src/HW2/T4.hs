module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last a) v = a :+ v
  (<>) (a :+ b) v = a :+ (b <> v)

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (This b)     = This (a <> b)
  (<>) (That a) (That b)     = That (a <> b)
  (<>) (This a) (That b)     = Both a b
  (<>) (That a) (This b)     = Both b a
  (<>) (Both a b) (This c)   = Both (a <> c) b
  (<>) (Both a b) (That c)   = Both a (b <> c)
  (<>) (This a) (Both b c)   = Both (a <> b) c
  (<>) (That a) (Both b c)   = Both b (a <> c)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  (<>) (DS "") val     = val
  (<>) val (DS "")     = val
  (<>) (DS v1) (DS v2) = DS $ v1 ++ ('.' : v2)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F v1) (F v2) = F $ v1 . v2

instance Monoid (Fun a) where
  mempty = F id
