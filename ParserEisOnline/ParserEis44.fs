namespace ParserEis
open Logging
open HtmlAgilityPack
open TypeAg
type ParserEis44(dir: string) =
      inherit AbstractParser()

      let mutable Urls = [ "http://zakupki.gov.ru/epz/order/quicksearch/search.html?morphology=on&sortDirection=false&recordsPerPage=_50&showLotsInfoHidden=false&fz44=on&af=on&currencyId=-1&regionDeleted=false&sortBy=PUBLISH_DATE&pageNumber=" ]
      let cPage = 9

      interface Iparser with

            override __.Parsing() =
                  __.Login()
                  match dir with
                  | "curr" -> __.GetTendersList()
                  | "last" | _ -> ()

      member private __.GetTendersList() =
          for u in Urls do
                for p in 1..cPage do
                      try
                            __.ParserPage <| sprintf "%s%d" u p
                      with ex -> Log.logger (ex)


      member private __.ParserPage(url) =
            match Download.DownloadString url with
            | "" -> Log.logger (sprintf "Empty string in ParserPage %s" url)
            | s -> let htmlDoc = new HtmlDocument()
                   htmlDoc.LoadHtml(s)
                   match htmlDoc.DocumentNode.SelectNodes("//div[@class = 'registerBox registerBoxBank margBtm20']") with
                   | null -> ()
                   | tens -> for t in tens do
                                   try
                                         __.ParserTender t url
                                   with ex -> Log.logger (ex)
                   ()

            ()

      member private __.ParserTender (node) (url) =
            let builder = DocumentBuilder()
            let result =
                  builder {
                        let! aTag = node.GsnAtrDocWithError ".//td[@class = 'descriptTenderTd']//a" "href" <| sprintf "href not found %s" url
                        printfn "%s" aTag
                        return "ok"
                  }
            match result with
            | Succ _ -> ()
            | Err e -> Log.logger e

            ()
