module HW5.Evaluator
    ( eval
    ) where

import HW5.Base
import Data.Ratio
import qualified Data.Text as T
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

-- TODO: fix this
unaryArray :: [HiFun]
unaryArray = [HiFunNot, HiFunLength, HiFunToUpper, HiFunToLower, HiFunReverse, HiFunTrim]

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval e = runExceptT (eval' e)

eval' :: Monad m => HiExpr -> ExceptT HiError m HiValue
eval' e = case e of
    HiExprValue v      -> pure v
    HiExprApply f args -> evalFun f args
    -- _ -> throwE HiErrorInvalidFunction

evalFun :: Monad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalFun f args = do
    f' <- eval' f
    case f' of
        HiValueFunction ff -> do
            case length args of
                1 -> if ff `elem` unaryArray then evalUn ff $ args!!0
                     else throwE HiErrorArityMismatch
                2 -> evalBin ff args
                3 -> evalTriple ff args
                _ -> throwE HiErrorArityMismatch
        HiValueString str -> do
            case length args of
                1 -> evalUnStringParse str $ args!!0
                2 -> evalBinStringParse str args
                _ -> throwE HiErrorArityMismatch
        _ -> throwE HiErrorInvalidFunction

evalUn :: Monad m => HiFun -> HiExpr -> ExceptT HiError m HiValue
evalUn f arg = do
    arg' <- eval' arg
    case arg' of
        HiValueBool _ -> evalUnParse f arg'
        HiValueString _ -> evalUnParse f arg'
        _ -> throwE HiErrorInvalidArgument

evalBin :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalBin f [l, r] = do
    l' <- eval' l
    r' <- eval' r
    if f == HiFunIf then throwE HiErrorArityMismatch
    else evalBinParse f l' r'
evalBin _ _ = throwE HiErrorArityMismatch

evalTriple :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalTriple f [c, l, r] = do
    case f of
        HiFunIf -> do
            c' <- eval' c
            case c' of
                HiValueBool _ -> evalTripleUniqueParse f c' l r
                _ -> throwE HiErrorInvalidArgument
        _ -> throwE HiErrorArityMismatch
evalTriple _ _ = throwE HiErrorArityMismatch

evalTripleUniqueParse :: Monad m => HiFun -> HiValue -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalTripleUniqueParse f c l r =
    case f of
        HiFunIf -> do
            case c of
                HiValueBool True -> (eval' l)
                HiValueBool False -> (eval' r)
                _ -> throwE HiErrorInvalidArgument
        _ -> throwE HiErrorInvalidFunction

evalBinParse :: Monad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalBinParse f l r = case (l, r, f) of
    (HiValueNumber l', HiValueNumber r', HiFunAdd) -> pure $ HiValueNumber (l' + r')
    (HiValueNumber l', HiValueNumber r', HiFunSub) -> pure $ HiValueNumber (l' - r')
    (HiValueNumber l', HiValueNumber r', HiFunMul) -> pure $ HiValueNumber (l' * r')
    (HiValueNumber _,  HiValueNumber 0,  HiFunDiv) -> throwE HiErrorDivideByZero
    (HiValueNumber l', HiValueNumber r', HiFunDiv) -> pure $ HiValueNumber (l' / r')
    (HiValueString l', HiValueString r', HiFunAdd) -> pure $ HiValueString (l' <> r')
    (HiValueString l', HiValueString r', HiFunDiv) -> pure $ HiValueString (l' <> T.pack "/" <> r')
    (HiValueString l', HiValueNumber r', HiFunMul) -> pure $ HiValueString (T.replicate (evalGetInt r') l')
    (HiValueBool True,  HiValueBool True,  HiFunAnd) -> pure $ HiValueBool True
    (HiValueBool _,     HiValueBool _,     HiFunAnd) -> pure $ HiValueBool False
    (HiValueBool True,  HiValueBool False, HiFunXor) -> pure $ HiValueBool True
    (HiValueBool False, HiValueBool True,  HiFunXor) -> pure $ HiValueBool True
    (HiValueBool _,     HiValueBool _,     HiFunXor) -> pure $ HiValueBool False
    (HiValueBool False, HiValueBool False, HiFunOr) -> pure $ HiValueBool False
    (HiValueBool _,     HiValueBool _,     HiFunOr) -> pure $ HiValueBool True
    (HiValueBool _, HiValueNumber _, HiFunLessThan) -> pure $ HiValueBool True
    (HiValueBool _, HiValueNumber _, HiFunGreaterThan) -> pure $ HiValueBool False
    (HiValueBool _, HiValueNumber _, HiFunNotLessThan) -> pure $ HiValueBool False
    (HiValueBool _, HiValueNumber _, HiFunNotGreaterThan) -> pure $ HiValueBool True
    (_, _, HiFunEquals) -> pure $ HiValueBool (l == r)
    (_, _, HiFunLessThan) -> pure $ HiValueBool (l < r)
    (_, _, HiFunGreaterThan) -> pure $ HiValueBool (l > r)
    (_, _, HiFunNotEquals) -> pure $ HiValueBool (l /= r)
    (_, _, HiFunNotLessThan) -> pure $ HiValueBool (l >= r)
    (_, _, HiFunNotGreaterThan) -> pure $ HiValueBool (l <= r)
    --_ -> pure $ HiValueBool False
    _ -> throwE HiErrorInvalidArgument

evalUnParse :: Monad m => HiFun -> HiValue -> ExceptT HiError m HiValue
evalUnParse f arg = case (arg, f) of
    (HiValueNull, HiFunLength) -> pure $ HiValueNumber 0
    (HiValueNull, HiFunToUpper) -> pure $ HiValueNull
    (HiValueString v, HiFunLength) -> pure $ HiValueNumber (toRational (T.length v))
    (HiValueString v, HiFunToUpper) -> pure $ HiValueString (T.toUpper v)
    (HiValueString v, HiFunToLower) -> pure $ HiValueString (T.toLower v)
    (HiValueString v, HiFunReverse) -> pure $ HiValueString (T.reverse v)
    (HiValueString v, HiFunTrim) -> pure $ HiValueString (T.strip v)
    (HiValueBool True, HiFunNot) -> pure $ HiValueBool False
    (HiValueBool _,    HiFunNot) -> pure $ HiValueBool True
    _ -> throwE HiErrorInvalidFunction


evalUnStringParse :: Monad m => T.Text -> HiExpr -> ExceptT HiError m HiValue
evalUnStringParse str arg = do
    arg' <- eval' arg
    case (arg', str) of
        (HiValueNumber n, _) ->
            if n < 0 then pure HiValueNull
            else if (denominator n) /= 1 then pure HiValueNull
            else if (evalGetInt n) >= (T.length str) then pure HiValueNull
            else pure $ HiValueString (T.singleton (T.index str (evalGetInt n)))
        _ -> pure $ HiValueNull

evalBinStringParse :: Monad m => T.Text -> [HiExpr] -> ExceptT HiError m HiValue
evalBinStringParse str [l, r] = do
    l' <- eval' l
    r' <- eval' r
    case (l', r') of
        (HiValueNumber a, HiValueNumber b) -> pure $ HiValueString (evalGetSubText str a b)
        (HiValueNumber a, HiValueNull) -> pure $ HiValueString (evalGetSubText str a sz)
        (HiValueNull, HiValueNumber b) -> pure $ HiValueString (evalGetSubText str 0 b)
        _ -> throwE HiErrorInvalidArgument
    where
        sz = toRational (T.length str)
evalBinStringParse _ _ = throwE HiErrorArityMismatch

evalGetSubText :: T.Text -> Rational -> Rational -> T.Text
evalGetSubText str a b =
    if (evalIntChecker a) && (evalIntChecker b) then
        evalGetSubText' str (evalGetModInt a sz) (evalGetModInt b sz) (T.length str)
    else T.empty
    where
        sz = T.length str

evalIntChecker :: Rational -> Bool
evalIntChecker v = if (denominator v) /= 1 then False else True

evalGetSubText' :: T.Text -> Int -> Int -> Int -> T.Text
evalGetSubText' str l r sz =
    if T.length s > 0 && T.length s > (sz - r - l) then
        T.take (r - l) s
    else
        T.empty
  where
    s = T.takeEnd (sz - l) str

evalGetInt :: Rational -> Int
evalGetInt n = fromIntegral (numerator n)

evalGetModInt :: Rational -> Int -> Int
evalGetModInt n m =
    if k < 0 then
        m + k
    else k
    where
        k = evalGetInt n