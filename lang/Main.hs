module Main where

import Symphony
import System.Environment (getArgs, getProgName)
import LexHashedLang
import ParHashedLang
import SkelHashedLang
import PrintHashedLang
import AbsHashedLang
import LayoutHashedLang

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do 
            content <- readFile fileName
            print $ pProblem . myLLexer $ content
            print $ parse content
