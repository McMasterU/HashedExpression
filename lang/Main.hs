module Main where

import AbsHashedLang
import Control.Monad.Except (runExceptT)
import LayoutHashedLang
import LexHashedLang
import ParHashedLang
import PrintHashedLang
import SkelHashedLang
import Symphony
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      content <- readFile fileName
      putStrLn "Checking your optimization problem...."
      res <- runExceptT $ parse content >>= checkSemanticAndGenCode
      case res of
        Left error -> print error
        Right proceed -> do
          putStrLn "Writing problem.c......"
          proceed "."
          putStrLn "Done"
    _ -> putStrLn "Please specify a Symphony source file"
