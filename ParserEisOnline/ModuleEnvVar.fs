namespace ParserEis

module S =
    type T =
        {
          TmpP : string
          Pref : string
          ConS : string
        }
    let mutable argTuple = Argument.Nan
    let mutable logFile = ""
    let mutable Token = ""
    let mutable Settings = {
        TmpP = ""
        Pref = ""
        ConS = ""
    }