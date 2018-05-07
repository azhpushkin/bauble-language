module Lib where

import Text.Pretty.Simple (pPrint)

import Syntax
import Parser
import Eval


process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> pPrint err
    Right ex -> mapM_ pPrint ex

parse = parseToplevel

runExpressions exprs = runInterpret emptyEnv exprs >> return ()