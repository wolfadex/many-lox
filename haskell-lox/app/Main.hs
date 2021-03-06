{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified System.Environment
import qualified System.Exit
import System.Exit (ExitCode(..))
import qualified System.IO
import qualified Text.Read

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


loxError :: Int -> String -> Error
loxError line message =
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


data TokenizerState = TokenizerState
  { stateCurrent :: Int
  , stateStart :: Int
  , stateSource :: String
  , stateLine :: Int
  }


scanTokens :: String -> Either [Error] [Token]
scanTokens source =
  scanTokensHelper
    (TokenizerState
      { stateCurrent = 0
      , stateStart = 0
      , stateSource = source
      , stateLine = 1
      }
    )
    []
    []


scanTokensHelper :: TokenizerState -> [Token] -> [Error] -> Either [Error] [Token]
scanTokensHelper state tokens errors =
  if isAtEnd state then
    case errors of
      [] ->
        Right $
          List.reverse
            (Token
              { tokType = EOF
              , tokLine = stateLine state
              , tokLexeme = ""
              }
              : tokens
            )
      _ -> Left $ List.reverse errors
  else
    case scanToken (state { stateStart = stateCurrent state }) of
      ( nextState, Left err ) -> scanTokensHelper nextState tokens (err : errors)
      ( nextState, Right (Just token) ) -> scanTokensHelper nextState (token : tokens) errors
      ( nextState, Right Nothing ) -> scanTokensHelper nextState tokens errors


scanToken :: TokenizerState -> ( TokenizerState, Either Error (Maybe Token) )
scanToken state =
  case drop (stateCurrent state) (stateSource state) of
    [] ->
      ( nextState
      , Left $
          loxError
            (stateLine state)
            ("Expected to find a char at " <> show (stateCurrent state) <> " index but found nothing.")  
      )
    -- Basic tokens
    '(':_ -> addToken LEFT_PAREN "("
    ')':_ -> addToken RIGHT_PAREN ")"
    '{':_ -> addToken LEFT_BRACE "{"
    '}':_ -> addToken RIGHT_BRACE "}"
    ',':_ -> addToken COMMA ","
    '.':_ -> addToken DOT "."
    '-':_ -> addToken MINUS "-"
    '+':_ -> addToken PLUS "+"
    ';':_ -> addToken SEMICOLON ";"
    '*':_ -> addToken STAR "*"
    -- Operators
    '!':'=':_ -> addToken BANG_EQUAL "!="
    '!':_ -> addToken BANG "!"
    '=':'=':_ -> addToken EQUAL_EQUAL "=="
    '=':_ -> addToken EQUAL "="
    '<':'=':_ -> addToken LESS_EQUAL "<="
    '<':_ -> addToken LESS "<"
    '>':'=':_ -> addToken GREATER_EQUAL ">="
    '>':_ -> addToken GREATER ">"
    -- Comments
    '/':'/':_ -> ( eatComment nextState, Right Nothing )
    '/':_ -> addToken SLASH "/"
    -- Whitespace
    ' ':_ -> noToken
    '\r':_ -> noToken
    '\t':_ -> noToken
    '\n':_ -> ( nextState { stateLine = stateLine nextState + 1 }, Right Nothing )
    -- String literals
    '"':_ -> tokenizeString nextState
    -- Everything else
    c:_ ->
      if Char.isDigit c then
        tokenizeNumber nextState
      else if Char.isAlpha c then
        tokenizeIdentifier nextState
      else
        ( nextState
        , Left $ loxError (stateLine state) ("Unsupported character: " <> show c)
        )
  where
    nextState = state { stateCurrent = stateCurrent state + 1 }
    noToken = ( nextState, Right Nothing )
    addToken type_ lexeme =
      ( nextState
      , Right $
          Just $
            Token { tokType = type_
                  , tokLine = stateLine nextState
                  , tokLexeme = lexeme
                  }      
      )


tokenizeIdentifier :: TokenizerState -> ( TokenizerState, Eight Error (Maybe Token) )
tokenizeIdentifier state =
    case drop (stateCurrent state) (stateSource state) of
      [] ->
        let text = slice (stateStart state) (stateCurrent state) (stateSource state)
        in
          ( state { stateCurrent = stateCurrent state + 1 }
          , Right $ Just $ Token
              { tokLine = stateLine state
              , tokType =
                case Map.lookup text keywordTokens of
                  Nothing -> IDENTIFIER text
                  Just tokenType -> tokenType
              , tokLexeme = text
              }
          )

      char:_ ->
        if Char.isAlphaNum char || char == '_' then
          tokenizeIdentifier $ state { stateCurrent = stateCurrent state + 1 }
        else
          let text = slice (stateStart state) (stateCurrent state) (stateSource state)
          in
            ( state { stateCurrent = stateCurrent state + 1 }
            , Right $ Just $ Token
                { tokLine = stateLine state
                , tokType =
                  case Map.lookup text keywordTokens of
                    Nothing -> IDENTIFIER text
                    Just tokenType -> tokenType
                , tokLexeme = text
                }
            )


keywordTokens : Map String TokenType
keywordTokens =
    Map.fromList
        [ ( "and", AND )
        , ( "class", CLASS )
        , ( "else", ELSE )
        , ( "false", FALSE )
        , ( "for", FOR )
        , ( "fun", FUN )
        , ( "if", IF )
        , ( "nil", NIL )
        , ( "or", OR )
        , ( "print", PRINT )
        , ( "return", RETURN )
        , ( "super", SUPER )
        , ( "this", THIS )
        , ( "true", TRUE )
        , ( "var", VAR )
        , ( "while", WHILE )
        ]


tokenizeNumber :: TokenizerState -> ( TokenizerState, Either Error (Maybe Token) )
tokenizeNumber state =
  case drop (stateCurrent state) (stateSource state) of
    [] ->
      ( state { stateCurrent = stateCurrent state + 1 }
      , let val = slice (stateStart state) (stateCurrent state + 1) (stateSource state)
        in
        case Text.Read.readMaybe val :: Maybe Float of
          Nothing -> Left $ loxError (stateLine state) ("Unexpectedly unable to parse number")
          Just num ->
            Right $
              Just $
                Token
                  { tokLine = stateLine state
                  , tokType = NUMBER num
                  , tokLexeme = val
                  }
      )
    '.':[] ->
      ( state { stateCurrent = stateCurrent state + 1 }
      , Left $ loxError (stateLine state) ("Unexpected period after number at EOF")
      )

    '.':_ ->
      tokenizeFloat $ state { stateCurrent = stateCurrent state + 1 }
      where
       tokenizeFloat s =
        case drop (stateCurrent s) (stateSource s) of
          [] ->
            ( s { stateCurrent = stateCurrent s + 1 }
            , let val = slice (stateStart s) (stateCurrent s) (stateSource s)
              in
              case Text.Read.readMaybe val :: Maybe Float of
                Nothing -> Left $ loxError (stateLine s) ("Unexpectedly unable to parse float")
                Just num ->
                  Right $
                    Just $
                      Token
                        { tokLine = stateLine s
                        , tokType = NUMBER num
                        , tokLexeme = val
                        }
            )

          char:_ ->
            if Char.isDigit char then
                tokenizeFloat $ s { stateCurrent = stateCurrent s + 1 }
            else
                ( s { stateCurrent = stateCurrent s + 1 }
                , let val = slice (stateStart s) (stateCurrent s) (stateSource s)
                  in
                  case Text.Read.readMaybe val :: Maybe Float of
                    Nothing -> Left $ loxError (stateLine s) ("Unexpectedly unable to parse float")
                    Just num ->
                      Right $
                        Just $
                          Token
                            { tokLine = stateLine s
                            , tokType = NUMBER num
                            , tokLexeme = val
                            }
                )
    char:_ ->
      if Char.isDigit char then
        tokenizeNumber $ state { stateCurrent = stateCurrent state + 1 }
      else
        ( state
        , let val = slice (stateStart state) (stateCurrent state) (stateSource state)
          in
          case Text.Read.readMaybe val :: Maybe Float of
            Nothing -> Left $ loxError (stateLine state) ("Unexpectedly unable to parse integer")
            Just num ->
              Right $
                Just $
                  Token
                    { tokLine = stateLine state
                    , tokType = NUMBER num
                    , tokLexeme = val
                    }
        )


tokenizeString :: TokenizerState -> ( TokenizerState, Either Error (Maybe Token) )
tokenizeString state =
  case drop (stateCurrent state) (stateSource state) of
    [] -> ( state, Left $ loxError (stateLine state) ("Unterminated string") )
    '\n':_ -> tokenizeString $ state { stateCurrent = stateCurrent state + 1, stateLine = stateLine state + 1 }
    '"':_ ->
      ( state { stateCurrent = stateCurrent state + 1 }
        , Right $
            Just $
              Token
                { tokLine = stateLine state
                , tokType =
                  STRING
                    (slice
                      (stateStart state + 1)
                      (stateCurrent state)
                      (stateSource state)
                    )
                , tokLexeme =
                    (slice
                      (stateStart state )
                      (stateCurrent state + 1)
                      (stateSource state)
                    )
                }
        )
    _ -> tokenizeString $ state { stateCurrent = stateCurrent state + 1 }


eatComment :: TokenizerState -> TokenizerState
eatComment state =
  case drop (stateCurrent state) (stateSource state) of
    [] -> state
    '\n':_ -> state { stateCurrent = stateCurrent state + 1 }
    _ -> eatComment $ state { stateCurrent = stateCurrent state + 1 }


slice :: Int -> Int -> [a] -> [a]
slice dropUntilIndex takeUntilIndex list =
  take (takeUntilIndex - dropUntilIndex) $ drop dropUntilIndex list


isAtEnd :: TokenizerState -> Bool
isAtEnd state =
  stateCurrent state >= List.length (stateSource state)