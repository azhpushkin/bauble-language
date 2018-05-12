module Calculations where

import qualified Data.Map.Strict as Map
import Data.Ratio

import Syntax


-- HELPER FUNCTIONS
numToRat (Integer int) = Rational (toRational int)
numToRat (Rational r)  = Rational r
numToRat (Double d)    = error ("Double (" ++ show d ++ ") cannot be converted to rational!")

numToDbl (Integer int) = Double (fromInteger int)
numToDbl (Rational r)  = Double (fromRational r)
numToDbl (Double d)    = Double d

-- Reduces rational to integer if possible
reduceRat (Integer int) = Integer int
reduceRat (Rational r)  = if (denominator r == 1)
                          then (Integer $ numerator r)
                          else (Rational r)
reduceRat (Double d)    = error ("Double (" ++ show d ++ ") cannot be reduced as rational!")


-- BINARY OPERATORS EVALUATION
evalBinOperator :: BinOperator -> Value -> Value -> Value

evalBinOperator Plus     (Number n1) (Number n2) = Number (binNumCalculation Plus n1 n2)
evalBinOperator Minus    (Number n1) (Number n2) = Number (binNumCalculation Minus n1 n2)
evalBinOperator Multiply (Number n1) (Number n2) = Number (binNumCalculation Multiply n1 n2)
evalBinOperator Divide   (Number n1) (Number n2) = Number (binNumCalculation Divide n1 n2)

evalBinOperator Less            (Number n1) (Number n2) = Boolean (binNumCompare Less n1 n2)
evalBinOperator Greater         (Number n1) (Number n2) = Boolean (binNumCompare Greater n1 n2)
evalBinOperator Equal           (Number n1) (Number n2) = Boolean (binNumCompare Equal n1 n2)
evalBinOperator LessOrEqual     (Number n1) (Number n2) = Boolean (binNumCompare LessOrEqual n1 n2)
evalBinOperator GreaterOrEqual  (Number n1) (Number n2) = Boolean (binNumCompare Greater n1 n2)
evalBinOperator NotEqual        (Number n1) (Number n2) = Boolean (binNumCompare Equal n1 n2)

evalBinOperator And (Boolean b1) (Boolean b2) = Boolean (b1 && b2)
evalBinOperator Or  (Boolean b1) (Boolean b2) = Boolean (b1 || b2)

evalBinOperator op l r = error $ "Operator "
                                 ++ show op
                                 ++ " called with unappropriate operands "
                                 ++ show l
                                 ++ " and "
                                 ++ show r

-- Helper function for comparing numbers
binNumCompare :: BinOperator -> Number -> Number -> Bool
binNumCompare Less     (Integer l) (Integer r) = (l < r)
binNumCompare Greater  (Integer l) (Integer r) = (l > r)
binNumCompare Equal    (Integer l) (Integer r) = (l == r)

binNumCompare Less     (Rational l) (Rational r) = (l < r)
binNumCompare Greater  (Rational l) (Rational r) = (l > r)
binNumCompare Equal    (Rational l) (Rational r) = (l == r)

binNumCompare Less     (Double l) (Double r) = (l < r)
binNumCompare Greater  (Double l) (Double r) = (l > r)
binNumCompare Equal    (Double l) (Double r) = (l == r)

binNumCompare op l              r@(Double _)   = binNumCompare op (numToDbl l) r
binNumCompare op l@(Double _)   r              = binNumCompare op l (numToDbl r)
binNumCompare op l              r@(Rational _) = binNumCompare op (numToRat l) r
binNumCompare op l@(Rational _) r              = binNumCompare op l (numToRat r)

binNumCompare LessOrEqual    l r = (binNumCompare Less l r) || (binNumCompare Equal l r)
binNumCompare GreaterOrEqual l r = (binNumCompare Greater l r) || (binNumCompare Equal l r)
binNumCompare NotEqual       l r = not (binNumCompare Equal l r)

binNumCompare op l r = error $ "binNumCompare called with operator "
                               ++ show op
                               ++ " and operands "
                               ++ show l
                               ++ " and "
                               ++ show r

-- Helper function for calculating operator results
binNumCalculation :: BinOperator -> Number -> Number -> Number

binNumCalculation Plus     (Integer l) (Integer r) = Integer (l + r)
binNumCalculation Minus    (Integer l) (Integer r) = Integer (l - r)
binNumCalculation Divide   (Integer l) (Integer r) = reduceRat $ Rational ((toRational l) / (toRational r))
binNumCalculation Multiply (Integer l) (Integer r) = Integer (l * r)

binNumCalculation Plus     (Rational l) (Rational r) = reduceRat $ Rational (l + r)
binNumCalculation Minus    (Rational l) (Rational r) = reduceRat $ Rational (l - r)
binNumCalculation Divide   (Rational l) (Rational r) = reduceRat $ Rational (l / r)
binNumCalculation Multiply (Rational l) (Rational r) = reduceRat $ Rational (l * r)

binNumCalculation Plus     (Double l) (Double r) = Double (l + r)
binNumCalculation Minus    (Double l) (Double r) = Double (l - r)
binNumCalculation Divide   (Double l) (Double r) = Double (l / r)
binNumCalculation Multiply (Double l) (Double r) = Double (l * r)

binNumCalculation op l              r@(Double _)   = binNumCalculation op (numToDbl l) r
binNumCalculation op l@(Double _)   r              = binNumCalculation op l (numToDbl r)
binNumCalculation op l              r@(Rational _) = binNumCalculation op (numToRat l) r
binNumCalculation op l@(Rational _) r              = binNumCalculation op l (numToRat r)

binNumCalculation op l r = error $ "binNumCalculation called with operator "
                                   ++ show op
                                   ++ " and operands "
                                   ++ show l
                                   ++ " and "
                                   ++ show r

-- UNARY OPERATORS EVALUATION
evalUnOperator :: UnOperator -> Value -> Value

evalUnOperator Not (Boolean b) = Boolean (not b)

evalUnOperator Negate (Number (Integer  i)) = Number (Integer  (-i))
evalUnOperator Negate (Number (Rational r)) = Number (Rational (-r))
evalUnOperator Negate (Number (Double   d)) = Number (Double   (-d))

evalUnOperator op v = error $ "Operator "
                              ++ show op
                              ++ " called with unappropriate operand "
                              ++ show v