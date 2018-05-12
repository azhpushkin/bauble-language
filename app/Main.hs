module Main where

import Lib

import Control.Monad.Trans
import System.Environment
import System.Console.Haskeline

import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"]      -> runREPL
    ["ast"]       -> runASTDebug
    [fname]       -> parseFile fname >> return ()
    _             -> help

help :: IO ()
help = putStr $ unlines
    [ "Usage: bauble [cmd | pathToFile]"
    , ""
    , "Commands:"
    , "  help      show this help text"
    , "  repl      start REPL"
    , "  ast       start REPL for AST debugging"
    ]


runASTDebug :: IO ()
runASTDebug = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing    -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop

runREPL = runASTDebug

parseFile file = do
  program  <- readFile file
  case parse program of
    Left e  -> pPrint e >> fail "parse error"
    Right exprs -> do
      putStrLn $ "----- Running " ++ file ++ " -----"
      startInterpreter exprs
      putStrLn $ "---------- END ----------"

