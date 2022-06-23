module Cli exposing (program)

import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Process


program : Process -> IO ()
program process =
    case process.argv of
        [ _, filename ] ->
            File.contentsOf filename
                |> IO.exitOnError identity
                |> IO.map run
                |> IO.exitOnError (List.map reportError >> String.join "\n")
                |> IO.andThen Process.print

        [ _ ] ->
            -- repl
            Process.print "repl"

        _ ->
            Process.logErr "Usage: elm-lox <file>\n"
                |> IO.andThen (\() -> Process.exit 64)


run : String -> Result (List Error) String
run source =
    case scanTokens source of
        Err errs ->
            Err errs

        Ok tokens ->
            List.map tokenToString tokens
                |> String.join "\n"
                |> Ok


type TokenType
    = -- Single-character tokens.
      LEFT_PAREN
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
    | -- One or two character tokens.
      BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    | -- Literals.
      IDENTIFIER
    | STRING
    | NUMBER
    | -- Keywords.
      AND
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


tokenTypeToString : TokenType -> String
tokenTypeToString tokenType =
    case tokenType of
        LEFT_PAREN ->
            "LEFT_PAREN"

        RIGHT_PAREN ->
            "RIGHT_PAREN"

        LEFT_BRACE ->
            "LEFT_BRACE"

        RIGHT_BRACE ->
            "RIGHT_BRACE"

        COMMA ->
            "COMMA"

        DOT ->
            "DOT"

        MINUS ->
            "MINUS"

        PLUS ->
            "PLUS"

        SEMICOLON ->
            "SEMICOLON"

        SLASH ->
            "SLASH"

        STAR ->
            "STAR"

        BANG ->
            "BANG"

        BANG_EQUAL ->
            "BANG_EQUAL"

        EQUAL ->
            "EQUAL"

        EQUAL_EQUAL ->
            "EQUAL_EQUAL"

        GREATER ->
            "GREATER"

        GREATER_EQUAL ->
            "GREATER_EQUAL"

        LESS ->
            "LESS"

        LESS_EQUAL ->
            "LESS_EQUAL"

        IDENTIFIER ->
            "IDENTIFIER"

        STRING ->
            "STRING"

        NUMBER ->
            "NUMBER"

        AND ->
            "AND"

        CLASS ->
            "CLASS"

        ELSE ->
            "ELSE"

        FALSE ->
            "FALSE"

        FUN ->
            "FUN"

        FOR ->
            "FOR"

        IF ->
            "IF"

        NIL ->
            "NIL"

        OR ->
            "OR"

        PRINT ->
            "PRINT"

        RETURN ->
            "RETURN"

        SUPER ->
            "SUPER"

        THIS ->
            "THIS"

        TRUE ->
            "TRUE"

        VAR ->
            "VAR"

        WHILE ->
            "WHILE"

        EOF ->
            "EOF"


type alias Token =
    { type_ : TokenType
    , lexeme : String

    -- , literal : Object -- This doesn't make sense in Elm, what do???
    , line : Int
    }


tokenToString : Token -> String
tokenToString token =
    tokenTypeToString token.type_
        ++ " "
        ++ token.lexeme



-- ++ " "
-- ++ token.literal


type alias TokenizerState =
    { start : Int
    , current : Int
    , line : Int
    , source : String
    }


scanTokens : String -> Result (List Error) (List Token)
scanTokens source =
    scanTokensHelper { start = 0, current = 0, line = 1, source = source } [] []


scanTokensHelper : TokenizerState -> List Token -> List Error -> Result (List Error) (List Token)
scanTokensHelper state tokens errors =
    if isAtEnd state then
        case errors of
            [] ->
                ({ type_ = EOF, lexeme = "", line = state.line } :: tokens)
                    |> List.reverse
                    |> Ok

            _ ->
                Err (List.reverse errors)

    else
        case scanToken { state | start = state.current } of
            ( nextState, Err err ) ->
                scanTokensHelper nextState tokens (err :: errors)

            ( nextState, Ok token ) ->
                scanTokensHelper nextState (token :: tokens) errors


scanToken : TokenizerState -> ( TokenizerState, Result Error Token )
scanToken state =
    let
        nextState =
            { state | current = state.current + 1 }
    in
    case String.uncons (String.dropLeft state.current state.source) of
        Nothing ->
            ( nextState
            , Err
                (error
                    { line = state.line
                    , message = "Expected to find a char at " ++ String.fromInt state.current ++ " index but found nothing."
                    }
                )
            )

        Just ( c, _ ) ->
            let
                addToken : TokenType -> ( TokenizerState, Result Error Token )
                addToken type_ =
                    ( nextState
                    , Ok
                        { type_ = type_
                        , line = nextState.line
                        , lexeme =
                            String.slice
                                nextState.start
                                nextState.current
                                nextState.source
                        }
                    )

                addTokenWithMatch : Char -> TokenType -> TokenType -> ( TokenizerState, Result Error Token )
                addTokenWithMatch toMatch matchType notMatchType =
                    if isAtEnd nextState then
                        addToken notMatchType

                    else
                        case String.uncons (String.dropLeft nextState.current nextState.source) of
                            Nothing ->
                                addToken notMatchType

                            Just ( char, _ ) ->
                                if char == toMatch then
                                    ( { nextState | current = nextState.current + 1 }
                                    , Ok
                                        { type_ = matchType
                                        , line = nextState.line
                                        , lexeme =
                                            String.slice
                                                nextState.start
                                                (nextState.current + 1)
                                                nextState.source
                                        }
                                    )

                                else
                                    addToken notMatchType
            in
            case c of
                '(' ->
                    addToken LEFT_PAREN

                ')' ->
                    addToken RIGHT_PAREN

                '{' ->
                    addToken LEFT_BRACE

                '}' ->
                    addToken RIGHT_BRACE

                ',' ->
                    addToken COMMA

                '.' ->
                    addToken DOT

                '-' ->
                    addToken MINUS

                '+' ->
                    addToken PLUS

                ';' ->
                    addToken SEMICOLON

                '*' ->
                    addToken STAR

                '!' ->
                    addTokenWithMatch '=' BANG_EQUAL BANG

                '=' ->
                    addTokenWithMatch '=' EQUAL_EQUAL EQUAL

                '<' ->
                    addTokenWithMatch '=' LESS_EQUAL LESS

                '>' ->
                    addTokenWithMatch '=' GREATER_EQUAL GREATER

                unsupportedChar ->
                    ( nextState
                    , Err
                        (error
                            { line = state.line
                            , message = "Unsupported character: " ++ String.fromChar unsupportedChar
                            }
                        )
                    )


type Error
    = Error
        { line : Int
        , message : String
        }


error : { line : Int, message : String } -> Error
error err =
    Error { line = err.line, message = err.message }


reportError : Error -> String
reportError (Error err) =
    "[line " ++ String.fromInt err.line ++ "] Error: " ++ err.message


repl =
    -- Process.print ">"
    --     |> IO.andThen (\() -> File.read File.stdIn)
    --     |> IO.andThen
    --         (\input ->
    --             if String.isEmpty input then
    --                 Process.print "Exiting"
    --             else
    --                 Process.print input
    --                     |> IO.andThen (\() -> repl)
    --         )
    Debug.todo "Figure out repl error"


isAtEnd : TokenizerState -> Bool
isAtEnd state =
    state.current >= String.length state.source
