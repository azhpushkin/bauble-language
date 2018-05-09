module Calculations where

import qualified Data.Map.Strict as Map
import Data.Either

import Syntax

toDouble = fromRational . toRational

calcBinOperator :: BinOperator -> Value -> Value -> Value
--calcBinOperator Plus (Integer l) (Integer r) = Integer (l + r)
--calcBinOperator Plus (Double l) (Integer r) = Double (l + fromInteger r)
--calcBinOperator Plus (Integer l) (Double r) = Double (fromInteger l + r)
--calcBinOperator Plus l r = error $ "Plus called with not numbers: " ++ (show l) ++ " and " ++ (show r)
--
--calcBinOperator Divide (Integer l) (Integer r) = Double (fromInteger l / fromInteger r)
--calcBinOperator Divide (Double l) (Integer r) = Double (l / fromInteger r)
--calcBinOperator Divide (Integer l) (Double r) = Double (fromInteger l / r)
--calcBinOperator Divide l r = error $ "Divide called with not numbers: " ++ (show l) ++ " and " ++ (show r)
--
--calcBinOperator Multiply (Integer l) (Integer r) = Integer (l * r)
--calcBinOperator Multiply (Double l) (Integer r) = Double (l * fromInteger r)
--calcBinOperator Multiply (Integer l) (Double r) = Double (fromInteger l * r)
--calcBinOperator Multiply l r = error $ "Multiply called with not numbers: " ++ (show l) ++ " and " ++ (show r)
--
--calcBinOperator Minus (Integer l) (Integer r) = Integer (l - r)
--calcBinOperator Minus (Double l) (Integer r) = Double (l - fromInteger r)
--calcBinOperator Minus (Integer l) (Double r) = Double (fromInteger l - r)
--calcBinOperator Minus l r = error $ "Minus called with not numbers: " ++ (show l) ++ " and " ++ (show r)
--
--
--calcBinOperator Less (Double l) (Double r) = Boolean (l < r)
--calcBinOperator Less (Integer l) (Double r) = Boolean (toDouble l < r)
--calcBinOperator Less (Double l) (Integer r) = Boolean (l < toDouble r)
--calcBinOperator Less (Integer l) (Integer r) = Boolean (l < r)
--calcBinOperator LessOrEqual (Double l) (Double r) = Boolean (l <= r)
--calcBinOperator LessOrEqual (Integer l) (Double r) = Boolean (toDouble l <= r)
--calcBinOperator LessOrEqual (Double l) (Integer r) = Boolean (l <= toDouble r)
--calcBinOperator LessOrEqual (Integer l) (Integer r) = Boolean (l <= r)
--calcBinOperator Greater (Double l) (Double r) = Boolean (l > r)
--calcBinOperator Greater (Integer l) (Double r) = Boolean (toDouble l > r)
--calcBinOperator Greater (Double l) (Integer r) = Boolean (l > toDouble r)
--calcBinOperator Greater (Integer l) (Integer r) = Boolean (l > r)
--calcBinOperator GreaterOrEqual (Double l) (Double r) = Boolean (l >= r)
--calcBinOperator GreaterOrEqual (Integer l) (Double r) = Boolean (toDouble l >= r)
--calcBinOperator GreaterOrEqual (Double l) (Integer r) = Boolean (l >= toDouble r)
--calcBinOperator GreaterOrEqual (Integer l) (Integer r) = Boolean (l >= r)
--calcBinOperator Equal (Double l) (Double r) = Boolean (l == r)
--calcBinOperator Equal (Integer l) (Double r) = Boolean (toDouble l == r)
--calcBinOperator Equal (Double l) (Integer r) = Boolean (l == toDouble r)
--calcBinOperator Equal (Integer l) (Integer r) = Boolean (l == r)
--calcBinOperator NotEqual (Double l) (Double r) = Boolean (l /= r)
--calcBinOperator NotEqual (Integer l) (Double r) = Boolean (toDouble l /= r)
--calcBinOperator NotEqual (Double l) (Integer r) = Boolean (l /= toDouble r)
--calcBinOperator NotEqual (Integer l) (Integer r) = Boolean (l /= r)
--
--
--calcBinOperator And (Boolean l) (Boolean r) = Boolean (l && r)
--calcBinOperator Or (Boolean l) (Boolean r) = Boolean (l || r)

calcBinOperator op l r = error $ "Operator " ++ show op ++ " called with " ++ show l ++ " and " ++ show r



calcUnOperator :: UnOperator -> Value -> Value
calcUnOperator Not (Boolean b) = Boolean (not b)
calcUnOperator Not v = error $ "Not called with non-bool: " ++ show v

--calcUnOperator Negate (Double d) = Double (-d)
--calcUnOperator Negate (Integer i) = Integer (-i)
calcUnOperator Negate v = error $ "Nerage called with non-number: " ++ show v