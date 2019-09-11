namespace ParserEis

module P =

    let parserExec (p : Iparser) = p.Parsing()

    let parserEis44 (dir : string) =
        try
            parserExec (ParserEis44(dir))
        with ex -> Logging.Log.logger ex
        
        ()