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
            Debug.todo "runPrompt"

        _ ->
            Process.logErr ("Usage: elm-lox <file>\n")
                |> IO.andThen (\() -> Process.exit 64)