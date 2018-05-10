module Syntax where

import qualified Data.Map.Strict as Map


data Number = Integer Integer
            | Rational Rational
            | Double Double
            deriving (Show, Ord, Eq)

numExpr :: Number -> Expr
numExpr = Value . Number


data Value = Number Number
           | Boolean Bool
           | Closure (Map.Map String Value) [String] [Expr]  -- Closure is not created directly
           | Null
           deriving (Show, Ord, Eq)


data Expr = Value Value
          | Variable String
          | Assign String Expr

          | Call Expr [Expr]
          | Function (Maybe String) [String] [Expr]  -- Defining a function
          | Return (Maybe Expr)  -- Only inside of Function

          | BinOperator BinOperator Expr Expr
          | UnOperator UnOperator Expr
          | If Expr [Expr] (Maybe [Expr])

          | While Expr [Expr]
          | Continue  -- Only inside of While
          | Break  -- Only inside of While

          | Print [Either Expr String]
          deriving (Show, Ord, Eq)

data BinOperator = Plus
                 | Minus
                 | Multiply
                 | Divide

                 | Less
                 | LessOrEqual
                 | Greater
                 | GreaterOrEqual
                 | Equal
                 | NotEqual

                 | And
                 | Or
                 deriving (Show, Ord, Eq)

data UnOperator = Not
                | Negate
                deriving (Show, Ord, Eq)
