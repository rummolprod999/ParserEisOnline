namespace ParserEis

open System
open System.IO
open System.Text
open System.Threading

module Logging =
    let locker = new Object()
    
    type Log() =
        static member logger ([<ParamArray>] args : Object []) =
            let mutable s = ""
            s <- DateTime.Now.ToString()
            args |> Seq.iter (fun x -> (s <- x.ToString() |> sprintf "%s %s" s))
            Monitor.Enter(locker)
            use sw = new StreamWriter(S.logFile, true, Encoding.Default)
            sw.WriteLine(s)
            Monitor.Exit(locker)