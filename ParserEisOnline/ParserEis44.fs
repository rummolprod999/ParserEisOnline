namespace ParserEis
open Logging
open HtmlAgilityPack
open TypeAg
open TypeSt
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
                        let urlTen = sprintf "http://zakupki.gov.ru%s" aTag
                        let! purNum = aTag.Get1Doc "regNumber=(\d+)$" <| sprintf "purNum not found %s" urlTen
                        let! status = node.GsnDocWithError ".//td[@class = 'tenderTd']//dt//span[contains(@class, 'noWrap')]" <| sprintf "status not found %s" urlTen
                        let! statusTen = status.Get1Doc @"(.+)/" <| sprintf "statusTen not found %s" urlTen
                        let! pubDateT = node.GsnDocWithError ".//li[label[. = 'Размещено:']]" <| sprintf "pubDateT not found %s" urlTen
                        let! datePub = pubDateT.Replace("Размещено:", "").Trim() .DateFromStringDoc("dd.MM.yyyy", sprintf "datePub not parse %s %s" pubDateT urlTen)
                        let! updDateT = node.GsnDocWithError ".//li[label[. = 'Обновлено:']]" <| sprintf "updDateT not found %s" urlTen
                        let! dateUpd = updDateT.Replace("Обновлено:", "").Trim() .DateFromStringDoc("dd.MM.yyyy", sprintf "dateUpd not parse %s %s" updDateT urlTen)
                        let doc = DocumentFz44(purNum, statusTen, datePub, dateUpd, urlTen)
                        try
                               doc.WorkerEntity()
                        with e -> Log.logger (e, urlTen)
                        return "ok"
                  }
            match result with
            | Succ _ -> ()
            | Err e -> Log.logger e

            ()
