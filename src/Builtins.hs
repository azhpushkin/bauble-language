module Builtins where


import Syntax
import Data.List

valueToString :: Value -> String
valueToString value =
  case value of
    Integer i -> (show i)
    Double d  -> (show d)
    Rational r  -> (show r)

    Boolean True -> "true"
    Boolean False -> "false"

    String s -> s

    Null -> "null"

    Closure (Just name) _ args _ -> ("<Closure \"" ++ name++ "\" with args " ++ show args ++ ">")
    Closure Nothing     _ args _ -> ("<Closure with args " ++ show args ++ ">")

    BuiltinFunction Print -> "<Built-in function print>"
    BuiltinFunction IsNull -> "<Built-in function isnull>"

    Array values -> ("[" ++ intercalate ", " (map valueToString values) ++ "]")

proceedBuiltin :: BuiltinFunction -> [Value] -> IO (Value)
proceedBuiltin Print values = putStrLn (unwords (map valueToString values)) >> return Null


proceedBuiltin IsNull [v] =
  case v of
    Null -> return (Boolean True)
    _    -> return (Boolean True)
proceedBuiltin IsNull args =
  error $ "isnull expected single argument, but got " ++ show args ++ "!"



