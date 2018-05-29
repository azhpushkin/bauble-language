{-# LANGUAGE ViewPatterns #-}

module Evaluation where

import Data.List (foldl')
import Control.Monad


import Lexer
import Syntax
import Environment
import Operators
import Builtins

-- Evaluate Expressions

evalExpression :: Expression -> Env -> IO (Value)

evalExpression (Value value) _ = return value

evalExpression (Variable var) env =
  case env `lookupEnv` var of
    Just v  -> return v
    Nothing -> error ("Variable " ++ var ++ " not found in scope!")


evalExpression (Function optName args body) env =
  case optName of
    Nothing     -> return (Closure optName env args body)
    (Just name) ->
      let closure = Closure optName (addToEnv name closure env) args body
      in return closure

evalExpression (BinaryOp operator leftExpr rightExpr) env = do
  leftOperand  <- (evalExpression leftExpr env)
  rightOperand <- (evalExpression rightExpr env)
  return (evalBinaryOp operator leftOperand rightOperand)

evalExpression (UnaryOp operator expr) env = do
  operand <- (evalExpression expr env)
  return (evalUnaryOp operator operand)

evalExpression (ArrayDeclare exprs) env = Array <$> (mapM (`evalExpression` env) exprs)

-- TODO: A lot of unhandled errors here, should fix
evalExpression (Subscript expr index) env
  | index < 0 = error ("Negative index " ++ show index ++ " not allowed!")
  | otherwise = do
    (Array values) <- (evalExpression expr env)
    return $ values !! (fromIntegral index)

evalExpression (Call expr argExprs) env = do
  callable <- (evalExpression expr env)
  case callable of
    BuiltinFunction builtin -> do
      argValues <- (mapM (`evalExpression` env) argExprs)
      proceedBuiltin builtin argValues
    Closure n closureEnv args body -> do
      argValues <- (mapM (`evalExpression` env) argExprs)
      if length args /= length argValues
      then error $ "Wrong number of arguments passed to " ++ show n ++ " : " ++ show args
      else do
        let newEnv = unionEnv (envFromList $ zip args argValues) closureEnv
        (_, Just callResult) <- runExpressions newEnv True Nothing body
        return callResult
    _ -> error $ "Non-function is called: " ++ show callable


-- Execute statements
-- TODO: all lower code should be rewritten as monadic

runExpressions :: Env             -- initial env
                  -> Bool         -- whether inside of function call
                  -> Maybe Statement   -- possible current while loop
                  -> [Statement]       -- statements to run
                  -> IO (Env, Maybe Value)  -- new env, result of runExpression
runExpressions env currCall currWhile stmts = do
  case stmts of
    [] ->
      case currWhile of
        Just (While cond whileExprs) -> do
          condValue <- evalExpression cond env
          case condValue of
            (Boolean True)  -> runExpressions env currCall currWhile whileExprs
            (Boolean False) -> return (env, Nothing)
        Nothing -> return (env, if currCall then (Just Null) else Nothing)

    (Expression e):exprs' -> do
      value <- (evalExpression e env)
      runExpressions env currCall currWhile exprs'

    (Assign var expr):exprs' -> do
      value  <- (evalExpression expr env)
      runExpressions (addToEnv var value env) currCall currWhile exprs'

    (Return (Just expr)):_ -> do
      value  <- (evalExpression expr env)
      return (env, Just value)
    (Return Nothing):_ -> return (env, Just Null)

    (Continue:_) -> runExpressions env currCall currWhile []
    (Break:_)    -> return (env, Nothing)


    ((If cond trueExprs falseExprs):exprs') -> do
      condValue <- evalExpression cond env
      case (condValue, falseExprs) of
        (Boolean True, _)                -> runExpressions env currCall currWhile (trueExprs  ++ exprs')
        (Boolean False, Just falseExprs) -> runExpressions env currCall currWhile (falseExprs ++ exprs')
        (Boolean False, Nothing)         -> runExpressions env currCall currWhile exprs'
        _ -> error $ "Condition of if-expression is not boolean but " ++ show condValue

    (newWhile@(While cond whileExprs):exprs') -> do
      condValue <- evalExpression cond env
      case condValue of
        (Boolean True) -> do
            (newEnv, _) <- runExpressions env currCall (Just newWhile) whileExprs
            runExpressions newEnv currCall currWhile exprs'
        (Boolean False) -> runExpressions env currCall currWhile exprs'
        _ -> error $ "Condition of while-expression is not boolean but " ++ show condValue

    (Import _ _ ):_ -> error "Imports are not implemented!"
