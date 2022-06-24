{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.List as List
import qualified System.Environment
import qualified System.Exit
import System.Exit (ExitCode(..))
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
runPrompt = do
  System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering
  repl

repl :: IO ()
repl = do
  putStr "> "
  input <- getLine
  if input == "" then
    pure ()
  else do
    run input
    repl


runFile :: FilePath -> IO ()
runFile filepath = do
  contents <- System.IO.readFile filepath
  run contents
  

run :: String -> IO ()
run source =
  case scanTokens source of
    Left errs ->
      putStrLn $ List.intercalate "\n" $ fmap errorToString errs

    Right tokens ->
      putStrLn $ List.intercalate "\n" $ fmap tokenToString tokens


data Error = Error
  { errLine :: Int
  , errMessage :: String
  }


error :: Int -> String -> Error
error line message =
    Error {errLine = line, errMessage = message}
    

errorToString :: Error -> String
errorToString err =
  "[line " <> (show $ errLine err) <> "] Error: " <> errMessage err


data Token = Token String


tokenToString :: Token -> String
tokenToString (Token token) =
  token


scanTokens :: String -> Either [Error] [Token]
scanTokens source =
    Right [Token source]