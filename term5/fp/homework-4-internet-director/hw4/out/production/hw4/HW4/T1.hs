module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad
import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES v) = ES (mapExcept (mapAnnotated f) . v)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState v = ES $ \s -> wrapExcept (v :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \s1 -> case f s1 of
    Error e -> Error e
    Success (ES v :# s2) -> v s2

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState v = ES $ \s -> Success (() :# v s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  a <*> b = Control.Monad.ap a b

instance Monad (ExceptState e s) where
  v >>= f = joinExceptState (fmap f v)

data EvaluationError = DivideByZero
  deriving Show

bOperation :: (Double -> Double -> Prim Double) ->
              Expr ->
              (Double -> Double -> Double) ->
              Expr ->
              ExceptState EvaluationError [Prim Double] Double
bOperation constr l oper r = do
  _l <- eval l
  _r <- eval r
  modifyExceptState (constr _l _r :)
  return $ oper _l  _r

uOperation :: (Double -> Prim Double) ->
              Expr ->
              (Double -> Double) ->
              ExceptState EvaluationError [Prim Double] Double
uOperation constr l oper = do
  _l <- eval l
  modifyExceptState (constr _l :)
  return $ oper _l

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val a) = do return a
eval (Op (Add l r)) = bOperation Add l (+) r
eval (Op (Sub l r)) = bOperation Sub l (-) r
eval (Op (Mul l r)) = bOperation Mul l (*) r
eval (Op (Abs l))   = uOperation Abs l (abs)
eval (Op (Sgn l))   = uOperation Sgn l (signum)
eval (Op (Div l r)) = do
  _l <- eval l
  _r <- eval r
  if _r == 0
    then throwExceptState DivideByZero
    else modifyExceptState (Div _l _r :)
  return $ _l / _r