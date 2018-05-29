module Operators where

import Syntax
import Data.Ratio


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


-- BINARY OPERATORS


evalBinaryOp :: BinaryOp -> Value -> Value -> Value


-- Math operators
evalBinaryOp Plus     (Integer l) (Integer r) = Integer (l + r)
evalBinaryOp Minus    (Integer l) (Integer r) = Integer (l - r)
evalBinaryOp Divide   (Integer l) (Integer r) = reduceRat $ Rational ((toRational l) / (toRational r))
evalBinaryOp Multiply (Integer l) (Integer r) = Integer (l * r)

evalBinaryOp Plus     (Rational l) (Rational r) = reduceRat $ Rational (l + r)
evalBinaryOp Minus    (Rational l) (Rational r) = reduceRat $ Rational (l - r)
evalBinaryOp Divide   (Rational l) (Rational r) = reduceRat $ Rational (l / r)
evalBinaryOp Multiply (Rational l) (Rational r) = reduceRat $ Rational (l * r)

evalBinaryOp Plus     (Double l) (Double r) = Double (l + r)
evalBinaryOp Minus    (Double l) (Double r) = Double (l - r)
evalBinaryOp Divide   (Double l) (Double r) = Double (l / r)
evalBinaryOp Multiply (Double l) (Double r) = Double (l * r)

evalBinaryOp op l              r@(Double _)   = evalBinaryOp op (numToDbl l) r
evalBinaryOp op l@(Double _)   r              = evalBinaryOp op l (numToDbl r)
evalBinaryOp op l              r@(Rational _) = evalBinaryOp op (numToRat l) r
evalBinaryOp op l@(Rational _) r              = evalBinaryOp op l (numToRat r)

evalBinaryOp Plus (Array values1) (Array values2)   = Array (values1 ++ values2)

-- Compare operators
evalBinaryOp Less     (Integer l) (Integer r) = Boolean (l < r)
evalBinaryOp Greater  (Integer l) (Integer r) = Boolean (l > r)
evalBinaryOp Equal    (Integer l) (Integer r) = Boolean (l == r)

evalBinaryOp Less     (Rational l) (Rational r) = Boolean (l < r)
evalBinaryOp Greater  (Rational l) (Rational r) = Boolean (l > r)
evalBinaryOp Equal    (Rational l) (Rational r) = Boolean (l == r)

evalBinaryOp Less     (Double l) (Double r) = Boolean (l < r)
evalBinaryOp Greater  (Double l) (Double r) = Boolean (l > r)
evalBinaryOp Equal    (Double l) (Double r) = Boolean (l == r)

evalBinaryOp op l              r@(Double _)   = evalBinaryOp op (numToDbl l) r
evalBinaryOp op l@(Double _)   r              = evalBinaryOp op l (numToDbl r)
evalBinaryOp op l              r@(Rational _) = evalBinaryOp op (numToRat l) r
evalBinaryOp op l@(Rational _) r              = evalBinaryOp op l (numToRat r)

evalBinaryOp LessOrEqual    l r = evalBinaryOp Or (evalBinaryOp Less l r) (evalBinaryOp Equal l r)
evalBinaryOp GreaterOrEqual l r = evalBinaryOp Or (evalBinaryOp Greater l r) (evalBinaryOp Equal l r)
evalBinaryOp NotEqual       l r = evalUnaryOp Not (evalBinaryOp Equal l r)

evalBinaryOp Equal    (Boolean l) (Boolean r) = Boolean (l == r)
evalBinaryOp Equal    (String l) (String r) = Boolean (l == r)
evalBinaryOp Equal    Null Null = Boolean True
evalBinaryOp Equal    (BuiltinFunction l) (BuiltinFunction r) = Boolean (l == r)
evalBinaryOp Equal    (Array lv) (Array rv) = Boolean (lv == rv)
evalBinaryOp Equal    _ _ = Boolean False

-- Boolean operators
evalBinaryOp And (Boolean b1) (Boolean b2) = Boolean (b1 && b2)
evalBinaryOp Or  (Boolean b1) (Boolean b2) = Boolean (b1 || b2)

evalBinaryOp op l r = error $ "evalBinaryOp called with operator "
                               ++ show op
                               ++ " and operands "
                               ++ show l
                               ++ " and "
                               ++ show r


evalUnaryOp :: UnaryOp -> Value -> Value
evalUnaryOp _ _ = error "asd"

evalUnOperator Not (Boolean b) = Boolean (not b)

evalUnOperator Negate (Integer  i) = Integer  (-i)
evalUnOperator Negate (Rational r) = Rational (-r)
evalUnOperator Negate (Double   d) = Double   (-d)

evalUnOperator op v = error $ "Operator "
                              ++ show op
                              ++ " called with unappropriate operand "
                              ++ show v