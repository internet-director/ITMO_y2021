module HW5.Base
    ( HiError(..)
    , HiExpr(..)
    , HiFun(..)
    , HiValue(..)
    ) where

import Data.Text

data HiFun =
    HiFunDiv
    | HiFunMul
    | HiFunAdd
    | HiFunSub
    | HiFunNot
    | HiFunXor
    | HiFunAnd
    | HiFunOr
    | HiFunEquals
    | HiFunLessThan
    | HiFunGreaterThan
    | HiFunNotEquals
    | HiFunNotLessThan
    | HiFunNotGreaterThan
    | HiFunIf
    | HiFunLength
    | HiFunToUpper
    | HiFunToLower
    | HiFunReverse
    | HiFunTrim
    deriving (Show, Ord, Eq)

data HiValue =
    HiValueNull
    | HiValueBool Bool
    | HiValueString Text
    | HiValueNumber Rational
    | HiValueFunction HiFun
    deriving (Show, Ord, Eq)

data HiExpr =
    HiExprValue HiValue
    | HiExprApply HiExpr [HiExpr]
    deriving (Show)

data HiError =
    HiErrorInvalidArgument
    | HiErrorInvalidFunction
    | HiErrorArityMismatch
    | HiErrorDivideByZero
    deriving (Show)