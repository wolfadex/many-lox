module Cli exposing (program)

import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Process


program : Process -> IO ()
program process =
    case Debug.log "args" process.argv of
        [ _, filename ] ->
            File.contentsOf filename
                |> IO.exitOnError identity
                |> IO.map run
                |> IO.exitOnError reportError
                |> IO.andThen Process.print

        [ _ ] ->
            -- repl
            Process.print "repl"

        _ ->
            Process.logErr "Usage: elm-lox <file>\n"
                |> IO.andThen (\() -> Process.exit 64)


run : String -> Result Error String
run source =
    scanTokens source
        |> List.map identity
        |> String.join "\n"
        |> Ok


scanTokens : String -> List String
scanTokens source =
    Debug.todo "tokenize!"


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
