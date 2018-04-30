module Syntax where

import qualified Data.Map as Map


data Value = Integer Integer
           | Rational (Integer, Integer)  -- Cannot be directly created
           | Double Double
           | Boolean Bool
           | Closure (Map.Map String Value) [String] [Expr]  -- Closure is not created directly
           | Null
           deriving (Show, Ord, Eq)



data Expr = Value Value
          | Variable String
          | Assign String Expr
          | Function [String] [Expr]  -- Defining a function
          | BinOperator BinOperator Expr Expr
          | UnOperator UnOperator Expr
          | If Expr [Expr] [Expr]
          | While Expr [Expr]
          | Return Expr
          | Call Expr [Expr]
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
