namespace ParserEis

open System.Data
open S
open Logging
open System
open MySql.Data.MySqlClient
open HtmlAgilityPack
open TypeAg
open TypeSt
open System

type DocumentFz44() =

      [<DefaultValue>] val mutable purNum: string
      [<DefaultValue>] val mutable status: string
      [<DefaultValue>] val mutable Url: string
      [<DefaultValue>] val mutable publishDate: DateTime
      [<DefaultValue>] val mutable updateDate: DateTime
      inherit AbstractDocument("ЕДИНАЯ ИНФОРМАЦИОННАЯ СИСТЕМА В СФЕРЕ ЗАКУПОК", "http://zakupki.gov.ru/", 1044)
      interface IDocument with
        override __.Worker() =
            __.WorkerEntity()

      new(purNum, status, publishDate, updateDate, url) as this = DocumentFz44()
                                                                  then this.purNum <- purNum
                                                                       this.status <- status
                                                                       this.publishDate <- publishDate
                                                                       this.updateDate <- updateDate
                                                                       this.Url <- url

      member __.WorkerEntity() =
            let builder = DocumentBuilder()
            use con = new MySqlConnection(Settings.ConS)
            let res =
                       builder {
                            con.Open()
                            let selectTend = sprintf "SELECT id_tender FROM %stender WHERE purchase_number = @purchase_number AND type_fz = @type_fz AND notice_version = @notice_version AND date_version = @date_version" Settings.Pref
                            let cmd: MySqlCommand = new MySqlCommand(selectTend, con)
                            cmd.Prepare()
                            cmd.Parameters.AddWithValue("@purchase_number", __.purNum) |> ignore
                            cmd.Parameters.AddWithValue("@type_fz", __.typeFz) |> ignore
                            cmd.Parameters.AddWithValue("@notice_version", __.status) |> ignore
                            cmd.Parameters.AddWithValue("@date_version", __.updateDate) |> ignore
                            let reader: MySqlDataReader = cmd.ExecuteReader()
                            if reader.HasRows then reader.Close()
                                                   return! Err ""
                            reader.Close()
                            let hTen: HtmlDocument = __.ReturnNavTender()
                            if hTen = null then return! Err <| sprintf "empty page tender was gotten %s" __.Url
                            let navT = (hTen.CreateNavigator()) :?> HtmlNodeNavigator
                            let hAtt = __.ReturnNavAttach()
                            if hAtt = null then return! Err <| sprintf "empty page attachments was gotten %s" __.Url
                            let! fulldatePub = navT.GsnDocWithError "//div[@class = 'public' and contains(., 'Размещено:')]" <| sprintf "fulldatePub not found %s" __.Url
                            let! datePubT = fulldatePub.Get1Doc @"(\d{2}\.\d{2}\.\d{4}\s\d{2}:\d{2})" <| sprintf "datePubT not found %s" __.Url
                            let! datePubLocal = datePubT.DateFromStringDoc("dd.MM.yyyy HH:mm", sprintf "datePubT not parse %s %s" datePubT __.Url)
                            let! uTc = fulldatePub.Get1Doc @"UTC([+\-]\d{1,2})" <| sprintf "uTc not found %s" __.Url
                            let tr, utc = Int32.TryParse(uTc)
                            if not tr then return! Err <| sprintf "utc offset not a number %s" __.Url
                            let msk = (utc - 3) * -1
                            let datePub = datePubLocal.AddHours(float msk)
                            
                            printfn "%s" fulldatePub
                            printfn "%O" datePubLocal
                            printfn "%O" datePub
                            let (cancelStatus, updated) = __.SetCancelStatus(con, __.updateDate, __.purNum)
                            return ""
                       }
            match res with
                | Succ _ -> ()
                | Err e when e = "" -> ()
                | Err r -> Log.logger r



      member private __.ReturnNavTender() =
           match Download.DownloadString __.Url with
           | "" -> null
           | s -> let htmlDoc = new HtmlDocument()
                  htmlDoc.LoadHtml(s)
                  htmlDoc
      
      member private __.ReturnNavAttach() =
          let urlAtt = __.Url.Replace("common-info", "documents")
          match Download.DownloadString urlAtt with
           | "" -> null
           | s -> let htmlDoc = new HtmlDocument()
                  htmlDoc.LoadHtml(s)
                  htmlDoc