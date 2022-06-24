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



data TokenType
    -- Single-character tokens
    = LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    -- One or two character tokens
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    -- Literals
    | IDENTIFIER String
    | STRING String
    | NUMBER Float
    -- Keywords
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | EOF


tokenTypeToString :: TokenType -> ( String, Maybe String )
tokenTypeToString tokenType =
  case tokenType of
    LEFT_PAREN -> ( "LEFT_PAREN", Nothing )
    RIGHT_PAREN -> ( "RIGHT_PAREN", Nothing )
    LEFT_BRACE -> ( "LEFT_BRACE", Nothing )
    RIGHT_BRACE -> ( "RIGHT_BRACE", Nothing )
    COMMA -> ( "COMMA", Nothing )
    DOT -> ( "DOT", Nothing )
    MINUS -> ( "MINUS", Nothing )
    PLUS -> ( "PLUS", Nothing )
    SEMICOLON -> ( "SEMICOLON", Nothing )
    SLASH -> ( "SLASH", Nothing )
    STAR -> ( "STAR", Nothing )
    BANG -> ( "BANG", Nothing )
    BANG_EQUAL -> ( "BANG_EQUAL", Nothing )
    EQUAL -> ( "EQUAL", Nothing )
    EQUAL_EQUAL -> ( "EQUAL_EQUAL", Nothing )
    GREATER -> ( "GREATER", Nothing )
    GREATER_EQUAL -> ( "GREATER_EQUAL", Nothing )
    LESS -> ( "LESS", Nothing )
    LESS_EQUAL -> ( "LESS_EQUAL", Nothing )
    IDENTIFIER literal -> ( "IDENTIFIER", Just literal )
    STRING literal -> ( "STRING", Just literal )
    NUMBER num -> ( "NUMBER", Just (show num) )
    AND -> ( "AND", Nothing )
    CLASS -> ( "CLASS", Nothing )
    ELSE -> ( "ELSE", Nothing )
    FALSE -> ( "FALSE", Nothing )
    FUN -> ( "FUN", Nothing )
    FOR -> ( "FOR", Nothing )
    IF -> ( "IF", Nothing )
    NIL -> ( "NIL", Nothing )
    OR -> ( "OR", Nothing )
    PRINT -> ( "PRINT", Nothing )
    RETURN -> ( "RETURN", Nothing )
    SUPER -> ( "SUPER", Nothing )
    THIS -> ( "THIS", Nothing )
    TRUE -> ( "TRUE", Nothing )
    VAR -> ( "VAR", Nothing )
    WHILE -> ( "WHILE", Nothing )
    EOF -> ( "EOF", Nothing )



data Token = Token
  { tokType :: TokenType
  , tokLine :: Int
  , tokLexeme :: String
  }


tokenToString :: Token -> String
tokenToString token =
  case tokenTypeToString (tokType token) of
    ( type_, Nothing ) -> type_ <> " " <> tokLexeme token
    ( type_, Just literal ) -> type_ <> " " <> tokLexeme token <> " " <> literal


scanTokens :: String -> Either [Error] [Token]
scanTokens source =
    Right [Token (IDENTIFIER source) 1 "carl"]