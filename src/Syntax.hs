module Syntax where

import qualified Data.Map.Strict as Map


data Number = Integer Integer
            | Rational Rational
            | Double Double
            deriving (Show, Ord, Eq)


data Value = Number Number
           | Boolean Bool
           | Closure (Map.Map String Value) [String] [Expr]  -- Closure is not created directly
           | Builtin Builtin
           | Null
           deriving (Show, Ord, Eq)


data Builtin = Print
             | Self
             deriving (Show, Ord, Eq)


data Expr = Value Value
          | Variable String
          | BuiltinRef Builtin
          | Assign String Expr

          | Call Expr [Expr]
          | Function [String] [Expr]  -- Defining a function
          | Return (Maybe Expr)  -- Only inside of Function

          | BinOperator BinOperator Expr Expr
          | UnOperator UnOperator Expr
          | If Expr [Expr] (Maybe [Expr])

          | While Expr [Expr]
          | Continue  -- Only inside of While
          | Break  -- Only inside of While
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
