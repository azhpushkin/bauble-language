module Lib where

import Text.Pretty.Simple (pPrint)

import Syntax
import Parser
import Evaluation


process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> pPrint err
    Right ex -> mapM_ pPrint ex

parse = parseToplevel

startInterpreter exprs = runExpressions emptyEnv False Nothing exprs >> return ()