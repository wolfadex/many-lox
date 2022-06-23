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
                |> IO.andThen (Debug.todo "runFile")

        [ _ ] ->
            repl

        _ ->
            Process.logErr "Usage: elm-lox <file>\n"
                |> IO.andThen (\() -> Process.exit 64)


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
