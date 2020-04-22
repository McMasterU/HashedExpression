module Symphony.Entry where

import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
import Options.Applicative
import Symphony.Common
import Symphony.Symphony
import System.Exit (exitFailure, exitSuccess)

data SymphonyOpts
  = SymphonyOpts
      { filePath :: String,
        noGenCode :: Bool,
        outputPath :: String
      }

symphonyOpts :: Parser SymphonyOpts
symphonyOpts =
  SymphonyOpts
    <$> strArgument
      ( metavar "INPUT"
          <> help "Symphony source file"
      )
    <*> switch
      ( short 'c'
          <> long "check-only"
          <> help "Only perform checking, do not generate code"
      )
    <*> strOption
      ( short 'o'
          <> long "output"
          <> value "."
          <> help "Where to write all the generated files, default \".\" "
      )

entry :: IO ()
entry = do
  let parserInfo =
        info
          (symphonyOpts <**> helper)
          (fullDesc <> progDesc "Symphony - symbolic computing & code generator for optimization solvers")
  opts <- execParser parserInfo
  symphony opts
  undefined

symphony :: SymphonyOpts -> IO ()
symphony (SymphonyOpts filePath noGenCode outputPath) = do
  content <- readFile filePath
  putStrLn "Checking your optimization problem...."
  res <- runExceptT $ do
    problem <- parse content
    validSymphonyInstance <- checkSemantic problem
    unless noGenCode $ do
      liftIO $ putStrLn "Generating files for optimization solvers......"
      generateCode outputPath validSymphonyInstance
      return ()
  case res of
    Left error -> do
      print error
      exitFailure
    Right _ -> do
      putStrLn "Done"
      exitSuccess
