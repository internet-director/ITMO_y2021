module HW5.Pretty
    ( prettyValue
    ) where

import HW5.Base
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Text()
import Data.Ratio


prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNull) = pretty "null"
prettyValue (HiValueBool v) = prettyBool v
prettyValue (HiValueString v) = pretty $ show v
prettyValue (HiValueNumber v) = prettyNumber v
prettyValue (HiValueFunction f) = prettyFunction f

prettyBool :: Bool -> Doc AnsiStyle
prettyBool v
    | v == True = pretty "true"
    | otherwise = pretty "false"

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber v =
    case denominator v of
        1 -> pretty (numerator v)
        _ -> prettyFrac (numerator v) (denominator v)

prettyFrac :: Integer -> Integer -> Doc AnsiStyle
prettyFrac v p =
    case prettyFracChecker p of -- TODO: optimisation
        True -> pretty $ (fromIntegral v :: Double) / (fromIntegral p :: Double)
        False -> do
            if abs v < p then pretty v <> pretty "/" <> pretty p
            else pretty (if v > 0 then k else (k + 1)) <> pretty (if v > 0 then "+" else "-") <> prettyFrac (abs v `mod` p) p
    where
        k = v `div` p

prettyFracChecker :: Integer -> Bool
prettyFracChecker v
    | v `mod` 5 == 0 = prettyFracChecker(v `div` 5)
    | v `mod` 2 == 0 = prettyFracChecker(v `div` 2)
    | otherwise = (1 == v)

prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction f = pretty $ prettyFunction' f

prettyFunction' :: HiFun -> [Char] -- TODO: map
prettyFunction' HiFunAdd = "add"
prettyFunction' HiFunSub = "sub"
prettyFunction' HiFunMul = "mul"
prettyFunction' HiFunDiv = "div"
prettyFunction' HiFunNot = "not"
prettyFunction' HiFunXor = "xor"
prettyFunction' HiFunAnd = "and"
prettyFunction' HiFunOr  = "or"
prettyFunction' HiFunIf = "if"
prettyFunction' HiFunEquals = "equals"
prettyFunction' HiFunLessThan = "less-than"
prettyFunction' HiFunGreaterThan = "greater-than"
prettyFunction' HiFunNotEquals = "not-equals"
prettyFunction' HiFunNotLessThan = "not-less-than"
prettyFunction' HiFunNotGreaterThan = "not-greater-than"
prettyFunction' HiFunLength = "length"
prettyFunction' HiFunToUpper = "to-upper"
prettyFunction' HiFunToLower = "to-lower"
prettyFunction' HiFunReverse = "reverse"
prettyFunction' HiFunTrim = "trim"