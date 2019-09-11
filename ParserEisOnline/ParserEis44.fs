namespace ParserEis

type ParserEis44(dir: string) =
      inherit AbstractParser()

      let mutable startUrl = []

      interface Iparser with

            override __.Parsing() =
                  __.Login()
                  match dir with
                  | "curr" -> __.GetTendersList()
                  | "last" | _ -> ()
      
      member private __.GetTendersList() = ()