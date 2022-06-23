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
    | STRING String
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


tokenTypeToString : TokenType -> ( String, Maybe String )
tokenTypeToString tokenType =
    case tokenType of
        LEFT_PAREN ->
            ( "LEFT_PAREN", Nothing )

        RIGHT_PAREN ->
            ( "RIGHT_PAREN", Nothing )

        LEFT_BRACE ->
            ( "LEFT_BRACE", Nothing )

        RIGHT_BRACE ->
            ( "RIGHT_BRACE", Nothing )

        COMMA ->
            ( "COMMA", Nothing )

        DOT ->
            ( "DOT", Nothing )

        MINUS ->
            ( "MINUS", Nothing )

        PLUS ->
            ( "PLUS", Nothing )

        SEMICOLON ->
            ( "SEMICOLON", Nothing )

        SLASH ->
            ( "SLASH", Nothing )

        STAR ->
            ( "STAR", Nothing )

        BANG ->
            ( "BANG", Nothing )

        BANG_EQUAL ->
            ( "BANG_EQUAL", Nothing )

        EQUAL ->
            ( "EQUAL", Nothing )

        EQUAL_EQUAL ->
            ( "EQUAL_EQUAL", Nothing )

        GREATER ->
            ( "GREATER", Nothing )

        GREATER_EQUAL ->
            ( "GREATER_EQUAL", Nothing )

        LESS ->
            ( "LESS", Nothing )

        LESS_EQUAL ->
            ( "LESS_EQUAL", Nothing )

        IDENTIFIER ->
            ( "IDENTIFIER", Nothing )

        STRING literal ->
            ( "STRING", Just literal )

        NUMBER ->
            ( "NUMBER", Nothing )

        AND ->
            ( "AND", Nothing )

        CLASS ->
            ( "CLASS", Nothing )

        ELSE ->
            ( "ELSE", Nothing )

        FALSE ->
            ( "FALSE", Nothing )

        FUN ->
            ( "FUN", Nothing )

        FOR ->
            ( "FOR", Nothing )

        IF ->
            ( "IF", Nothing )

        NIL ->
            ( "NIL", Nothing )

        OR ->
            ( "OR", Nothing )

        PRINT ->
            ( "PRINT", Nothing )

        RETURN ->
            ( "RETURN", Nothing )

        SUPER ->
            ( "SUPER", Nothing )

        THIS ->
            ( "THIS", Nothing )

        TRUE ->
            ( "TRUE", Nothing )

        VAR ->
            ( "VAR", Nothing )

        WHILE ->
            ( "WHILE", Nothing )

        EOF ->
            ( "EOF", Nothing )


type alias Token =
    { type_ : TokenType
    , lexeme : String
    , line : Int
    }


tokenToString : Token -> String
tokenToString token =
    let
        ( type_, literal ) =
            tokenTypeToString token.type_
    in
    type_
        ++ " "
        ++ token.lexeme
        ++ (case literal of
                Nothing ->
                    ""

                Just lit ->
                    " " ++ lit
           )


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

            ( nextState, Ok (Just token) ) ->
                scanTokensHelper nextState (token :: tokens) errors

            ( nextState, Ok Nothing ) ->
                scanTokensHelper nextState tokens errors


scanToken : TokenizerState -> ( TokenizerState, Result Error (Maybe Token) )
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
                noToken : ( TokenizerState, Result Error (Maybe Token) )
                noToken =
                    ( nextState, Ok Nothing )

                addToken : TokenType -> ( TokenizerState, Result Error (Maybe Token) )
                addToken type_ =
                    ( nextState
                    , Ok
                        (Just
                            { type_ = type_
                            , line = nextState.line
                            , lexeme =
                                String.slice
                                    nextState.start
                                    nextState.current
                                    nextState.source
                            }
                        )
                    )

                addTokenWithMatch : Char -> TokenType -> TokenType -> ( TokenizerState, Result Error (Maybe Token) )
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
                                        (Just
                                            { type_ = matchType
                                            , line = nextState.line
                                            , lexeme =
                                                String.slice
                                                    nextState.start
                                                    (nextState.current + 1)
                                                    nextState.source
                                            }
                                        )
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

                '/' ->
                    let
                        ( nState, isMatch ) =
                            if isAtEnd nextState then
                                ( nextState, False )

                            else
                                case String.uncons (String.dropLeft nextState.current nextState.source) of
                                    Nothing ->
                                        ( nextState, False )

                                    Just ( char, _ ) ->
                                        if char /= '/' then
                                            ( nextState, False )

                                        else
                                            ( { nextState | current = nextState.current + 1 }, True )
                    in
                    if isMatch then
                        ( eatComment nState, Ok Nothing )

                    else
                        addToken SLASH

                ' ' ->
                    noToken

                -- \r
                '\u{000D}' ->
                    noToken

                '\t' ->
                    noToken

                '\n' ->
                    ( { nextState | line = nextState.line + 1 }
                    , Ok Nothing
                    )

                '"' ->
                    tokenizeString nextState

                unsupportedChar ->
                    ( nextState
                    , Err
                        (error
                            { line = state.line
                            , message = "Unsupported character: " ++ String.fromChar unsupportedChar
                            }
                        )
                    )


tokenizeString : TokenizerState -> ( TokenizerState, Result Error (Maybe Token) )
tokenizeString state =
    case String.uncons (String.dropLeft state.current state.source) of
        Nothing ->
            ( state, Err (error { line = state.line, message = "Unterminated string" }) )

        Just ( '\n', _ ) ->
            tokenizeString { state | current = state.current + 1, line = state.line + 1 }

        Just ( '"', _ ) ->
            ( { state | current = state.current + 1 }
            , Ok
                (Just
                    { line = state.line
                    , type_ =
                        STRING
                            (String.slice
                                (state.start + 1)
                                state.current
                                state.source
                            )
                    , lexeme =
                        String.slice
                            state.start
                            (state.current + 1)
                            state.source
                    }
                )
            )

        Just _ ->
            tokenizeString { state | current = state.current + 1 }


eatComment : TokenizerState -> TokenizerState
eatComment state =
    case String.uncons (String.dropLeft state.current state.source) of
        Nothing ->
            state

        Just ( '\n', _ ) ->
            { state | current = state.current + 1 }

        Just _ ->
            eatComment { state | current = state.current + 1 }


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
