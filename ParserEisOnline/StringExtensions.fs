namespace ParserEis

open System
open System.Globalization
open System.Text.RegularExpressions

module TypeSt =
    let (|RegexMatch2|_|) (pattern: string) (input: string) =
        let result = Regex.Match(input, pattern)
        if result.Success then
            match (List.tail [ for g in result.Groups -> g.Value ]) with
            | fst :: snd :: [] -> Some(fst, snd)
            | _ -> None
        else None

    let (|RegexMatch1|_|) (pattern: string) (input: string) =
        let result = Regex.Match(input, pattern)
        if result.Success then
            match (List.tail [ for g in result.Groups -> g.Value ]) with
            | fst :: [] -> Some(fst)
            | _ -> None
        else None

    type System.String with

        member this.DateFromString(pat: string) =
            try
                Some(DateTime.ParseExact(this, pat, CultureInfo.InvariantCulture))
            with ex -> None

        member this.DateFromStringDoc(pat: string, exc: string) =
            match this.DateFromString(pat) with
            | None -> Err(exc)
            | Some d -> Succ(d)

        member this.DateFromStringDocMin(pat: string) =
            match this.DateFromString(pat) with
            | None -> Succ(DateTime.MinValue)
            | Some d -> Succ(d)

        member this.DateFromStringDocNow(pat: string) =
            match this.DateFromString(pat) with
            | None -> Succ(DateTime.Now)
            | Some d -> Succ(d)

        member this.Get1FromRegexp(regex: string): string option =
            match this with
            | RegexMatch1 regex gr1 -> Some(gr1)
            | _ -> None

        member this.Get1FromRegexpOrDefaul(regex: string): string =
            match this with
            | RegexMatch1 regex gr1 -> gr1
            | _ -> ""

        member this.Get1OptionalDoc(regex: string) =
            match this.Get1FromRegexp(regex) with
            | None -> Succ("")
            | Some e -> Succ(e.Trim())

        member this.Get1Doc (regex: string) (exc: string) =
            match this.Get1FromRegexp(regex) with
            | None -> Err(exc)
            | Some e -> Succ(e.Trim())

        member this.Get2FromRegexp(regex: string): (string * string) option =
            match this with
            | RegexMatch2 regex (gr1, gr2) -> Some(gr1, gr2)
            | _ -> None

        member this.Get2FromRegexpOptional(regex: string, exc: string) =
            match this with
            | RegexMatch2 regex (gr1, gr2) -> Succ(gr1, gr2)
            | _ -> Err(exc)

        member this.GetPriceFromString(?template): string =
            let templ = defaultArg template @"([\d, ]+)"
            match this.Get1FromRegexp templ with
            | Some x -> Regex.Replace(x.Replace(",", ".").Trim(), @"\s+", "")
            | None -> ""

        member this.DateFromStringRus(pat: string) =
            try
                Some(DateTime.ParseExact(this, pat, CultureInfo.CreateSpecificCulture("ru-RU")))
            with ex ->
                printfn "%O" this
                printfn "%O" pat
                None

        member this.RegexReplace() = Regex.Replace(this, @"\s+", " ")
        member this.RegexDeleteWhitespace() = Regex.Replace(this, @"\s+", "")
        member this.RegexCutWhitespace() = Regex.Replace(this, @"\s+", " ")

        member this.GetNmck() =
            let tmp = this.Replace(",", ".").RegexDeleteWhitespace()
            tmp

        member this.ReplaceDate() =
            if this.Contains("января") then this.Replace(" января ", ".01.")
            elif this.Contains("февраля") then this.Replace(" февраля ", ".02.")
            elif this.Contains("марта") then this.Replace(" марта ", ".03.")
            elif this.Contains("апреля") then this.Replace(" апреля ", ".04.")
            elif this.Contains("мая") then this.Replace(" мая ", ".05.")
            elif this.Contains("июня") then this.Replace(" июня ", ".06.")
            elif this.Contains("июля") then this.Replace(" июля ", ".07.")
            elif this.Contains("августа") then this.Replace(" августа ", ".08.")
            elif this.Contains("сентября") then this.Replace(" сентября ", ".09.")
            elif this.Contains("октября") then this.Replace(" октября ", ".10.")
            elif this.Contains("ноября") then this.Replace(" ноября ", ".11.")
            elif this.Contains("декабря") then this.Replace(" декабря ", ".12.")
            else this

        member this.ReplaceDateAriba() =
            if this.Contains("янв") then this.Replace(" янв ", ".01.")
            elif this.Contains("фев") then this.Replace(" фев ", ".02.")
            elif this.Contains("мар") then this.Replace(" мар ", ".03.")
            elif this.Contains("апр") then this.Replace(" апр ", ".04.")
            elif this.Contains("мая") then this.Replace(" мая ", ".05.")
            elif this.Contains("июн") then this.Replace(" июн ", ".06.")
            elif this.Contains("июл") then this.Replace(" июл ", ".07.")
            elif this.Contains("авг") then this.Replace(" авг ", ".08.")
            elif this.Contains("сен") then this.Replace(" сен ", ".09.")
            elif this.Contains("окт") then this.Replace(" окт ", ".10.")
            elif this.Contains("ноя") then this.Replace(" ноя ", ".11.")
            elif this.Contains("дек") then this.Replace(" дек ", ".12.")
            else this

        member this.ReplaceDateAsgor() =
            if this.Contains("Января") then this.Replace("Января", "01")
            elif this.Contains("Февраля") then this.Replace("Февраля", "02")
            elif this.Contains("Марта") then this.Replace("Марта", "03")
            elif this.Contains("Апреля") then this.Replace("Апреля", "04")
            elif this.Contains("Мая") then this.Replace("Мая", "05")
            elif this.Contains("Июня") then this.Replace("Июня", "06")
            elif this.Contains("Июля") then this.Replace("Июля", "07")
            elif this.Contains("Августа") then this.Replace("Августа", "08")
            elif this.Contains("Сентября") then this.Replace("Сентября", "09")
            elif this.Contains("Октября") then this.Replace("Октября", "10")
            elif this.Contains("Ноября") then this.Replace("Ноября", "11")
            elif this.Contains("Декабря") then this.Replace("Декабря", "12")
            else this
