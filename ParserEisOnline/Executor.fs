namespace ParserEis

open Logging
open System

module Executor =
    let arguments = "44 curr, 223 curr"

    let parserArgs = function
                     | [| "44"; "curr" |] -> S.argTuple <- Argument.Eis44("curr")
                     | [| "223"; "curr" |] -> S.argTuple <- Argument.Eis223("curr")
                     | _ -> printf "Bad arguments, use %s" arguments
                            Environment.Exit(1)
    let parser = function
                 | Eis44 d ->
                     try
                        P.parserEis44 d
                     with e -> Log.logger e
                 | _ -> ()