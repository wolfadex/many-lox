module Main where

import qualified System.Exit
import System.Exit (ExitCode(..))
import qualified System.Environment
import qualified System.IO

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    [] ->
      runPrompt

    [filepath] -> 
      runFile filepath
    _ -> do
      putStrLn "Usage: haskell-lox <script>"
      System.Exit.exitWith (ExitFailure 64)


runPrompt :: IO ()
runPrompt =
  undefined


runFile :: FilePath -> IO ()
runFile filepath = do
  contents <- System.IO.readFile filepath
  putStrLn contents
  