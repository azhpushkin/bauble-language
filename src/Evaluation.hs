module Evaluation where

import qualified Data.Map.Strict as Map
import Data.Either

import Syntax
import Calculations

type Env = (Map.Map String Value)

emptyEnv :: Map.Map String Value
emptyEnv = Map.fromList []



runExpressions :: Env             -- initial env
                  -> Bool         -- whether inside of function call
                  -> Maybe Expr   -- current while loop
                  -> [Expr]       -- expressions to run
                  -> IO (Env, Maybe Value)  -- new env, result of runExpression
runExpressions env currCall currWhile exprs = do
  case exprs of
    [] ->
      case currWhile of
        Just (While cond whileExprs) -> do
          (condValue, _) <- evalExpr env cond
          case condValue of
            (Boolean True)  -> runExpressions env currCall currWhile whileExprs
            (Boolean False) -> return (env, Nothing)
        Nothing -> return (env, if currCall then (Just Null) else Nothing)

    (Return (Just expr)):_ -> do
      (value, _) <- evalExpr env expr
      return (env, Just value)
    (Return Nothing):_ -> return (env, Nothing)

    (Continue:_) -> runExpressions env currCall currWhile []
    (Break:_)    -> return (env, Nothing)


    ((If cond trueExprs falseExprs):exprs') -> do
      (condValue, _) <- evalExpr env cond
      case (condValue, falseExprs) of
        (Boolean True, _)                -> runExpressions env currCall currWhile (trueExprs  ++ exprs')
        (Boolean False, Just falseExprs) -> runExpressions env currCall currWhile (falseExprs ++ exprs')
        (Boolean False, Nothing)         -> runExpressions env currCall currWhile exprs'
        _ -> error $ "Condition of if-expression is not boolean but " ++ show condValue

    (newWhile@(While cond whileExprs):exprs') -> do
      (condValue, _) <- evalExpr env cond
      case condValue of
        (Boolean True) -> do
            (newEnv, _) <- runExpressions env currCall (Just newWhile) whileExprs
            runExpressions newEnv currCall currWhile exprs'
        (Boolean False) -> runExpressions env currCall currWhile exprs'
        _ -> error $ "Condition of while-expression is not boolean but " ++ show condValue

    (expr:exprs') -> do
      (value, newEnv) <- evalExpr env expr
      runExpressions newEnv currCall currWhile exprs'


evalExpr :: Env -> Expr -> IO (Value, Env)
evalExpr env (Value v) = return (v, env)

evalExpr env (Variable var) =
  case env Map.!? var of
    Just v  -> return (v, env)
    Nothing -> fail ("Variable " ++ var ++ " not found in scope!")

evalExpr env (Assign var expr) = do
  (value, newEnv) <- evalExpr env expr
  return (value, Map.insert var value newEnv)

evalExpr env (Function maybeName args exprs) = do
  case maybeName of
    Nothing -> return (Closure maybeName env args exprs, env)
    (Just name) -> do
      let closure = Closure maybeName (Map.insert name closure env) args exprs
      return (closure, env)

evalExpr env (BinOperator op left right) = do
  (lvalue, _) <- evalExpr env left
  (rvalue, _) <- evalExpr env right
  return $ (evalBinOperator op lvalue rvalue, env)

evalExpr env (UnOperator op arg) = do
  (value, _) <- evalExpr env arg
  return $ (evalUnOperator op value, env)

evalExpr env (Print []) = putStrLn "" >> return (Null, env)
evalExpr env (Print (expr:exprs)) = do
  case expr of
    Left expr -> do
      (value, newEnv) <- evalExpr env expr
      case value of
        Closure (Just name) env args exprs -> putStr $ "<Closure \"" ++ name++ "\" with args " ++ show args ++ ">"
        Closure Nothing     env args exprs -> putStr $ "<Closure with args " ++ show args ++ ">"
        Number (Integer i)  -> putStr $ "<Integer " ++ show i ++ ">"
        Number (Double d)   -> putStr $ "<Double " ++ show d ++ ">"
        Number (Rational r) -> putStr $ "<Double " ++ show r ++ ">"
        Boolean b           -> putStr $ "<Boolean " ++ show b ++ ">"
        Null                -> putStr $ "<Null>"
    Right str -> putStr str

  evalExpr env (Print exprs)


evalExpr env (Call func args) = do
  (callable, _) <- evalExpr env func
  case callable of
    Closure maybeName savedEnv argsNames exprs -> do
      if length args /= length argsNames
      then error $ "Wrong number of arguments passed to " ++ show maybeName ++ " : " ++ show args
      else do
        argsEvalRes <- mapM (evalExpr env) args
        let argsValues = map (\(v, env) -> v) argsEvalRes
        let argsMap = Map.fromList $ zip argsNames argsValues
        (_, callResult) <- runExpressions (Map.union argsMap savedEnv) True Nothing exprs
        case callResult of
          Just value -> return (value, env)
          Nothing    -> error $ "Call " ++ show maybeName ++ " of args (" ++ show args ++ ") returned not value!"
    _ -> fail $ "Non-function is called: " ++ show callable

evalExpr _ v = fail $ show v ++ " expression is not allowed to use with evalExpr!"

