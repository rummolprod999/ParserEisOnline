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
                            let! datePubLocal = datePubT.DateFromStringDoc ("dd.MM.yyyy HH:mm", sprintf "datePubT not parse %s %s" datePubT __.Url)
                            let! uTc = fulldatePub.Get1Doc @"UTC([+\-]\d{1,2})" <| sprintf "uTc not found %s" __.Url
                            let tr, utc = Int32.TryParse(uTc)
                            if not tr then return! Err <| sprintf "utc offset not a number %s" __.Url
                            let msk = (utc - 3) * -1
                            let datePub = datePubLocal.AddHours(float msk)
                            let! printForm = navT.GsnAtrDocWithError "//a[contains(@href, 'printForm')]" "href" <| sprintf "printForm not found %s" __.Url
                            let printForm = sprintf "http://zakupki.gov.ru%s" printForm
                            let noticeVersion = navT.GsnDocWithError "//td[text() = 'Этап закупки']/following-sibling::td" <| sprintf "noticeVersion not found %s" __.Url
                            let numVersion = 0
                            let (cancelStatus, updated) = __.SetCancelStatus(con, __.updateDate, __.purNum)
                            let! purchaseObjectInfo = navT.GsnDocWithError "//td[text() = 'Наименование объекта закупки']/following-sibling::td" <| sprintf "purchaseObjectInfo not found %s" __.Url
                            let! organizerFullName = navT.GsnDocWithError "//td[text() = 'Организация, осуществляющая размещение']/following-sibling::td" <| sprintf "organizerFullName not found %s" __.Url
                            let! organizerPostAddress = navT.GsnDocWithError "//td[text() = 'Почтовый адрес']/following-sibling::td" <| sprintf "organizerPostAddress not found %s" __.Url
                            let! organizerFactAddress = navT.GsnDocWithError "//td[text() = 'Место нахождения']/following-sibling::td" <| sprintf "organizerFactAddress not found %s" __.Url
                            let organizerInn = ""
                            let! organizerContact = navT.GsnDocWithError "//td[text() = 'Ответственное должностное лицо']/following-sibling::td" <| sprintf "organizerContact not found %s" __.Url
                            let! organizerEmail = navT.GsnDocWithError "//td[text() = 'Адрес электронной почты']/following-sibling::td" <| sprintf "organizerEmail not found %s" __.Url
                            let! organizerFax = navT.GsnDocWithError "//td[text() = 'Факс']/following-sibling::td" <| sprintf "organizerFax not found %s" __.Url
                            let! organizerPhone = navT.GsnDocWithError "//td[text() = 'Номер контактного телефона']/following-sibling::td" <| sprintf "organizerPhone not found %s" __.Url
                            let idOrganizer = ref 0
                            let idCustomer = ref 0
                            if organizerFullName <> "" then
                                let selectOrg = sprintf "SELECT id_organizer FROM %sorganizer WHERE full_name = @full_name" Settings.Pref
                                let cmd3 = new MySqlCommand(selectOrg, con)
                                cmd3.Prepare()
                                cmd3.Parameters.AddWithValue("@full_name", organizerFullName) |> ignore
                                let reader = cmd3.ExecuteReader()
                                match reader.HasRows with
                                | true ->
                                    reader.Read() |> ignore
                                    idOrganizer := reader.GetInt32("id_organizer")
                                    reader.Close()
                                | false ->
                                    reader.Close()
                                    let addOrganizer = sprintf "INSERT INTO %sorganizer SET full_name = @full_name, contact_person = @contact_person, post_address = @post_address, fact_address = @fact_address, contact_phone = @contact_phone, inn = @inn, contact_email = @contact_email, contact_fax = @contact_fax" Settings.Pref
                                    let cmd5 = new MySqlCommand(addOrganizer, con)
                                    cmd5.Parameters.AddWithValue("@full_name", organizerFullName) |> ignore
                                    cmd5.Parameters.AddWithValue("@contact_person", organizerContact) |> ignore
                                    cmd5.Parameters.AddWithValue("@post_address", organizerPostAddress) |> ignore
                                    cmd5.Parameters.AddWithValue("@fact_address", organizerFactAddress) |> ignore
                                    cmd5.Parameters.AddWithValue("@contact_phone", organizerPhone) |> ignore
                                    cmd5.Parameters.AddWithValue("@inn", organizerInn) |> ignore
                                    cmd5.Parameters.AddWithValue("@contact_email", organizerEmail) |> ignore
                                    cmd5.Parameters.AddWithValue("@contact_fax", organizerFax) |> ignore
                                    cmd5.ExecuteNonQuery() |> ignore
                                    idOrganizer := int cmd5.LastInsertedId
                                    ()
                            let! placingWayName = navT.GsnDocWithError "//td[contains(., 'Способ определения поставщика')]/following-sibling::td" <| sprintf "placingWayName not found %s" __.Url
                            let idPlacingWay = ref <| __.GetPlacingWay con placingWayName Settings
                            let idEtp = ref <| __.GetEtp con Settings
                            
                            
                            let! dateEndT = navT.GsnDoc "//td[contains(., 'Дата и время окончания подачи заявок')]/following-sibling::td"
                            let! dateEndLocal = dateEndT.Replace("в ", "").DateFromStringDocMin "dd.MM.yyyy HH:mm"
                            let mutable dateEnd = dateEndLocal
                            if dateEndLocal <> DateTime.MinValue then
                                dateEnd <- dateEndLocal.AddHours(float msk)
                            else
                                let! dateEndT = navT.GsnDoc "//td[contains(., 'Дата и время окончания срока подачи заявок')]/following-sibling::td"
                                let! dateEndLocal = dateEndT.Replace("в ", "").DateFromStringDocMin "dd.MM.yyyy HH:mm"
                                if dateEndLocal <> DateTime.MinValue then
                                    dateEnd <- dateEndLocal.AddHours(float msk)
                            if dateEnd = DateTime.MinValue then return! Err <| sprintf "dateEnd is not found %s" __.Url
                                    
                                    
                            let! dateScoringT = navT.GsnDoc "//td[contains(., 'Дата окончания срока рассмотрения первых частей заявок участников')]/following-sibling::td"
                            let! dateScoringLocal = dateScoringT.Replace("в ", "").DateFromStringDocMin "dd.MM.yyyy"
                            let mutable dateScoring = dateScoringLocal
                            if dateScoringLocal <> DateTime.MinValue then
                                dateScoring <- dateScoringLocal.AddHours(float msk)
                                
                            else
                                let! dateScoringT = navT.GsnDoc "//td[contains(., 'Дата и время рассмотрения и оценки первых частей заявок')]/following-sibling::td"
                                let! dateScoringLocal = dateScoringT.Replace("в ", "").DateFromStringDocMin "dd.MM.yyyy HH:mm"
                                if dateScoringLocal <> DateTime.MinValue then
                                    dateScoring <- dateScoringLocal.AddHours(float msk)
                            
                            
                            
                            let! biddingDateT = navT.GsnDoc "//td[contains(., 'Дата проведения аукциона в электронной форме')]/following-sibling::td"
                            let! biddingTimeT = navT.GsnDoc "//td[contains(., 'Время проведения аукциона')]/following-sibling::td"

                            let biddingDateT = sprintf "%s %s" biddingDateT biddingTimeT
                            let! dateBiddingLocal = biddingDateT.DateFromStringDocMin "dd.MM.yyyy HH:mm"
                            let mutable dateBidding = dateBiddingLocal
                            if dateBidding <> DateTime.MinValue then
                                dateBidding <- dateBiddingLocal.AddHours(float msk)

                            let idTender = ref 0
                            let insertTender = String.Format ("INSERT INTO {0}tender SET id_xml = @id_xml, purchase_number = @purchase_number, doc_publish_date = @doc_publish_date, href = @href, purchase_object_info = @purchase_object_info, type_fz = @type_fz, id_organizer = @id_organizer, id_placing_way = @id_placing_way, id_etp = @id_etp, end_date = @end_date, scoring_date = @scoring_date, bidding_date = @bidding_date, cancel = @cancel, date_version = @date_version, num_version = @num_version, notice_version = @notice_version, xml = @xml, print_form = @print_form, id_region = @id_region", Settings.Pref)
                            let cmd9 = new MySqlCommand(insertTender, con)
                            cmd9.Prepare()
                            cmd9.Parameters.AddWithValue("@id_xml", __.purNum) |> ignore
                            cmd9.Parameters.AddWithValue("@purchase_number", __.purNum) |> ignore
                            cmd9.Parameters.AddWithValue("@doc_publish_date", datePub) |> ignore
                            cmd9.Parameters.AddWithValue("@href", __.Url) |> ignore
                            cmd9.Parameters.AddWithValue("@purchase_object_info", purchaseObjectInfo) |> ignore
                            cmd9.Parameters.AddWithValue("@type_fz", __.typeFz) |> ignore
                            cmd9.Parameters.AddWithValue("@id_organizer", !idOrganizer) |> ignore
                            cmd9.Parameters.AddWithValue("@id_placing_way", !idPlacingWay) |> ignore
                            cmd9.Parameters.AddWithValue("@id_etp", !idEtp) |> ignore
                            cmd9.Parameters.AddWithValue("@end_date", dateEnd) |> ignore
                            cmd9.Parameters.AddWithValue("@scoring_date", dateScoring) |> ignore
                            cmd9.Parameters.AddWithValue("@bidding_date", dateBidding) |> ignore
                            cmd9.Parameters.AddWithValue("@cancel", cancelStatus) |> ignore
                            cmd9.Parameters.AddWithValue("@date_version", __.updateDate) |> ignore
                            cmd9.Parameters.AddWithValue("@num_version", numVersion) |> ignore
                            cmd9.Parameters.AddWithValue("@notice_version", noticeVersion) |> ignore
                            cmd9.Parameters.AddWithValue("@xml", __.Url) |> ignore
                            cmd9.Parameters.AddWithValue("@print_form", printForm) |> ignore
                            cmd9.Parameters.AddWithValue("@id_region", 0) |> ignore
                            cmd9.ExecuteNonQuery() |> ignore
                            idTender := int cmd9.LastInsertedId
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
