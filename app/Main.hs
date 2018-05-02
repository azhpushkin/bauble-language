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
    [] -> runREPL
    [fname] -> parseFile fname >> return ()

runREPL :: IO ()
runREPL = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing    -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop


--processFile :: String -> IO ()
--processFile fname = readFile fname >>= parse >> return ()

parseFile file = do
  program  <- readFile file
  case parse program of
    Left e  -> pPrint e >> fail "parse error"
    Right r -> pPrint r >> return r
