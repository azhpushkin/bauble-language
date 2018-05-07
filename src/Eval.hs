module Eval where

import qualified Data.Map.Strict as Map
import Data.Either

import Syntax
import Calculations

type Env = (Map.Map String Value)

emptyEnv :: Map.Map String Value
emptyEnv = Map.fromList []



runInterpret :: Env -> [Expr] -> IO (Env)
runInterpret env exprs = do
  case exprs of
    [] -> return env
    (expr:exprs') -> do
      (value, newEnv) <- evalExpr env expr
      runInterpret newEnv exprs'


evalExpr :: Env -> Expr -> IO (Value, Env)
evalExpr env (Value v) = return (v, env)


evalExpr env (Variable var) =
  case env Map.!? var of
    Just v  -> return (v, env)
    Nothing -> fail ("Variable " ++ var ++ " not found in scope!")

evalExpr env (Assign var expr) = do
  (value, newEnv) <- evalExpr env expr
  return (value, Map.insert var value newEnv)


evalExpr env (Function args exprs) = do
  let closure = Closure (Map.insert "self" closure env) args exprs
  return (closure, env)


evalExpr env (BinOperator op left right) = do
  (lvalue, _) <- evalExpr env left
  (rvalue, _) <- evalExpr env right
  return $ (calcBinOperator op lvalue rvalue, env)

evalExpr env (UnOperator op arg) = do
  (value, _) <- evalExpr env arg
  return $ (calcUnOperator op value, env)


evalExpr env (If cond trueExprs falseExprs) = do
  (condValue, _) <- evalExpr env cond
  case condValue of
    Boolean True -> do
      newEnv <- runInterpret env trueExprs
      return (Null, newEnv)
    Boolean False -> do
      case falseExprs of
        Nothing -> return (Null, env)
        Just exprs -> do
          newEnv <- runInterpret env trueExprs
          return (Null, newEnv)
    v -> fail $ "Non boolean value passed to if: " ++ show v


evalExpr env (Print []) = putStrLn "" >> return (Null, env)
evalExpr env (Print (expr:exprs)) = do
  case expr of
    Left expr -> do
      (value, newEnv) <- evalExpr env expr
      case value of
        Closure _ args exprs -> putStr $ "<Closure with args " ++ show args ++ ">"
        Integer i -> putStr $ "<Integer " ++ show i ++ ">"
        Double d -> putStr $ "<Double " ++ show d ++ ">"
        Boolean b -> putStr $ "<Boolean " ++ show b ++ ">"
        Null -> putStr $ "<Null>"
    Right str -> putStr str

  evalExpr env (Print exprs)

evalExpr env (While cond exprs) = do
  (condValue, _) <- evalExpr env cond
  case condValue of
    Boolean True -> do
      (newEnv, canContinue) <- runExprsInWhile env exprs
      if canContinue
      then evalExpr newEnv (While cond exprs)
      else return $ (Null, newEnv)
    Boolean False -> return $ (Null, env)
    v -> fail $ "Non boolean value passed to if: " ++ show v


evalExpr env (Call func args) = do
  (callable, newEnv) <- evalExpr env func
  case callable of
    Closure savedEnv argsNames exprs -> do
      if length args /= length argsNames
      then fail $ "Wrong number of arguments passed: " ++ show args
      else do
        argsEvalRes <- mapM (evalExpr newEnv) args
        let argsValues = map (\(v, env) -> v) argsEvalRes
        let argsMap = Map.fromList $ zip argsNames argsValues
        value <- runExprsInFunc (Map.union argsMap savedEnv) exprs
        return (value, env)
    _ -> fail $ "Non-function is called: " ++ show callable

evalExpr _ v = fail $ show v ++ " expression is not allowed here!"


runExprsInFunc :: Env -> [Expr] -> IO (Value)
runExprsInFunc env exprs = do
  case exprs of
    [] -> return Null
    (Return (Just expr)):_ -> do
      (value, _) <- evalExpr env expr
      return value
    (Return Nothing):_ -> return Null
    expr:exprs' -> do
      (value, newEnv) <- evalExpr env expr
      res <- runExprsInFunc newEnv exprs'
      return res

runExprsInWhile :: Env -> [Expr] -> IO (Env, Bool)
runExprsInWhile env exprs = do
  case exprs of
    (Continue:_) -> return (env, True)
    (Break:_) -> return (env, False)
    (expr:exprs') -> do
      (_, newEnv) <- evalExpr env expr
      res <- runExprsInWhile newEnv exprs'
      return res
    [] -> return (env, True)


