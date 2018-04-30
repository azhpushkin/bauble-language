module Main where

import Lib

import Control.Monad.Trans
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing    -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop