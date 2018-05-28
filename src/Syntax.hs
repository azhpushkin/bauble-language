module Syntax where

import qualified Data.Map.Strict as Map


data BuiltinFunction = Print
                     | IsNull
                     deriving (Show, Ord, Eq)

data Value = Integer Integer
           | Rational Rational
           | Double Double
           | Boolean Bool
           | String String
           | Null

           -- Closure is never created directly,
           -- this is the result of function declaration
           | Closure (Maybe String)  -- Optional function name
                     (Map.Map String Value)  -- Saved environment
                     [String]  -- List of argument names
                     [Statement]  -- Function body
           | BuiltinFunction BuiltinFunction  -- Functions like print, length

           | Array [Expression]
           deriving (Show, Ord, Eq)

data Expression = Value Value
                | Variable String  -- name of variable
                | Call Expression [Expression]  -- Callable and arguments

                -- Function declaration expression
                | Function (Maybe String)  -- Optional name of function
                           [String]  -- List of argument names
                           [Statement]  -- Function body

                -- Operators and array subscription
                | BinaryOp BinaryOp Expression Expression
                | UnaryOp UnaryOp Expression
                | Subscript Expression Integer

                deriving (Show, Ord, Eq)


data Statement = Expression Expression
               | Assign String Expression
               | While Expression [Statement]
               | If Expression [Statement] (Maybe [Statement])
               | Import [String] (Maybe String)

               -- Function body only
               | Return (Maybe Expression)
               | Nonlocal String

               -- While loop body only
               | Continue
               | Break
               deriving (Show, Ord, Eq)


data BinaryOp = Plus
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

data UnaryOp = Not
             | Negate
             deriving (Show, Ord, Eq)
