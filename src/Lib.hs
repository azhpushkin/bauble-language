module Lib where

import Parser

import Text.Pretty.Simple (pPrint)


process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> pPrint err
    Right ex -> mapM_ pPrint ex

parse = parseToplevel

