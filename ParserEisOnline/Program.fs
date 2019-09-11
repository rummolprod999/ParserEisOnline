namespace ParserEis

open System
module EntryPoint =
    [<EntryPoint>]
    let main argv =
       if argv.Length = 0 then
                    printf "Bad arguments, use %s" Executor.arguments
                    Environment.Exit(1)
       
       Executor.parserArgs argv
       Stn.getSettings()
       Logging.Log.logger "Начало парсинга"
       Executor.parser (S.argTuple)
       Logging.Log.logger (sprintf "Добавили тендеров %d" AbstractDocument.Add)
       Logging.Log.logger (sprintf "Обновили тендеров %d" AbstractDocument.Upd)
       0 // return an integer exit code
