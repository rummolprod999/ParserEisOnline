namespace ParserEis

module TypeAg =
    type HtmlAgilityPack.HtmlNode with

        member this.Gsn(s: string) =
            match this.SelectSingleNode(s) with
            | null -> ""
            | e -> e.InnerText.Trim()

        member this.GsnDoc(xpath: string) =
            match this.SelectSingleNode(xpath) with
            | null -> Succ("")
            | e -> Succ(e.InnerText.Trim())

        member this.GsnDocWithError (xpath: string) (err: string) =
            match this.SelectSingleNode(xpath) with
            | null -> Err(err)
            | e -> Succ(e.InnerText.Trim())

        member this.GsnAtr (s: string) (atr: string) =
            match this.SelectSingleNode(s) with
            | null -> ""
            | e ->
                match e.Attributes.[atr] with
                | null -> ""
                | at -> at.Value.Trim()

        member this.GsnAtrDoc (xpath: string) (atr: string) =
            match this.SelectSingleNode(xpath) with
            | null -> Succ("")
            | e ->
                match e.Attributes.[atr] with
                | null -> Succ("")
                | at -> Succ(at.Value.Trim())

        member this.GsnAtrDocWithError (xpath: string) (atr: string) (err: string) =
            match this.SelectSingleNode(xpath) with
            | null -> Err(err)
            | e ->
                match e.Attributes.[atr] with
                | null -> Err(err)
                | at -> Succ(at.Value.Trim())

        member this.getAttrWithoutException (atr: string) =
            match this.Attributes.[atr] with
            | null -> ""
            | at -> at.Value.Trim()

    type HtmlAgilityPack.HtmlNodeNavigator with

        member this.Gsn(s: string) =
            match this.SelectSingleNode(s) with
            | null -> ""
            | e -> e.Value.Trim()

        member this.GsnAtr (s: string) (atr: string) =
            match this.SelectSingleNode(s) with
            | null -> ""
            | e ->
                match e.GetAttribute(atr, "") with
                | null -> ""
                | at -> at.Trim()

        member this.GsnDoc(xpath: string) =
            match this.SelectSingleNode(xpath) with
            | null -> Succ("")
            | e -> Succ(e.Value.Trim())

        member this.GsnDocWithError (xpath: string) (err: string) =
            match this.SelectSingleNode(xpath) with
            | null -> Err(err)
            | e -> Succ(e.Value.Trim())

        member this.GsnAtrDoc (xpath: string) (atr: string) =
            match this.SelectSingleNode(xpath) with
            | null -> Succ("")
            | e ->
                match e.GetAttribute(atr, "") with
                | null -> Succ("")
                | at -> Succ(at.Trim())

        member this.GsnAtrDocWithError (xpath: string) (atr: string) (err: string) =
            match this.SelectSingleNode(xpath) with
            | null -> Err(err)
            | e ->
                match e.GetAttribute(atr, "") with
                | null -> Err(err)
                | at -> Succ(at.Trim())
