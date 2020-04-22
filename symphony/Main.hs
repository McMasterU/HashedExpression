module Main where

import Control.Monad.Except (runExceptT)
import Symphony.Entry
import System.Environment (getArgs, getProgName)
import System.Exit

main :: IO ()
main = entry
