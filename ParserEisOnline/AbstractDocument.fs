namespace ParserEis

open MySql.Data.MySqlClient
open System.Data
open System.Text
open System.Text.RegularExpressions
open System

[<AbstractClass>]
type AbstractDocument() =
    [<DefaultValue>] val mutable etpName: string
    [<DefaultValue>] val mutable etpUrl: string
    [<DefaultValue>] val mutable typeFz: int

    new(n, u, f) as this = AbstractDocument()
                           then
                                this.etpName <- n
                                this.etpUrl <- u
                                this.typeFz <- f
       

    member this.EtpName = this.etpName
    member this.EtpUrl = this.etpUrl
    static member val Add : int = 0 with get, set
    static member val Upd : int = 0 with get, set
    abstract GetEtp: con:MySqlConnection -> S.T -> int
    abstract GetPlacingWay: con:MySqlConnection -> string -> S.T -> int
    
    override this.GetEtp (con: MySqlConnection) (stn: S.T): int =
        let selectEtp = sprintf "SELECT id_etp FROM %setp WHERE name = @name AND url = @url" stn.Pref
        let cmd6 = new MySqlCommand(selectEtp, con)
        cmd6.Prepare()
        cmd6.Parameters.AddWithValue("@name", this.EtpName) |> ignore
        cmd6.Parameters.AddWithValue("@url", this.EtpUrl) |> ignore
        let reader3 = cmd6.ExecuteReader()
        match reader3.HasRows with
        | true ->
            reader3.Read() |> ignore
            let idEtp = reader3.GetInt32("id_etp")
            reader3.Close()
            idEtp
        | false ->
            reader3.Close()
            let insertEtp = sprintf "INSERT INTO %setp SET name= @name, url= @url, conf=0" stn.Pref
            let cmd7 = new MySqlCommand(insertEtp, con)
            cmd7.Prepare()
            cmd7.Parameters.AddWithValue("@name", this.etpName) |> ignore
            cmd7.Parameters.AddWithValue("@url", this.etpUrl) |> ignore
            cmd7.ExecuteNonQuery() |> ignore
            let idEtp = int cmd7.LastInsertedId
            idEtp
    
    override this.GetPlacingWay (con: MySqlConnection) (placingWayName: string) (stn: S.T): int =
        let selectPlacingWay = sprintf "SELECT id_placing_way FROM %splacing_way WHERE name= @name" stn.Pref
        let cmd6 = new MySqlCommand(selectPlacingWay, con)
        cmd6.Prepare()
        cmd6.Parameters.AddWithValue("@name", placingWayName) |> ignore
        let reader3 = cmd6.ExecuteReader()
        match reader3.HasRows with
        | true ->
            reader3.Read() |> ignore
            let idPlacingWay = reader3.GetInt32("id_placing_way")
            reader3.Close()
            idPlacingWay
        | false ->
            reader3.Close()
            let conf = this.GetConformity placingWayName
            let insertPlacingWay =
                sprintf "INSERT INTO %splacing_way SET name= @name, conformity = @conformity" stn.Pref
            let cmd7 = new MySqlCommand(insertPlacingWay, con)
            cmd7.Prepare()
            cmd7.Parameters.AddWithValue("@name", placingWayName) |> ignore
            cmd7.Parameters.AddWithValue("@conformity", conf) |> ignore
            cmd7.ExecuteNonQuery() |> ignore
            let idPlacingWay = int cmd7.LastInsertedId
            idPlacingWay
    
    member this.GetConformity(s: string): int =
        let sLower = s.ToLower()
        match sLower with
        | s when s.Contains("открыт") -> 5
        | s when s.Contains("аукцион") -> 1
        | s when s.Contains("котиров") -> 2
        | s when s.Contains("предложен") -> 3
        | s when s.Contains("единств") -> 4
        | _ -> 6
    
    member this.AddVerNumber (con: MySqlConnection) (pn: string) (stn: S.T): unit =
            let verNum = ref 1
            let selectTenders =
                sprintf
                    "SELECT id_tender FROM %stender WHERE purchase_number = @purchaseNumber AND type_fz = @type_fz ORDER BY UNIX_TIMESTAMP(date_version) ASC"
                    stn.Pref
            let cmd1 = new MySqlCommand(selectTenders, con)
            cmd1.Prepare()
            cmd1.Parameters.AddWithValue("@purchaseNumber", pn) |> ignore
            cmd1.Parameters.AddWithValue("@type_fz", this.typeFz) |> ignore
            let dt1 = new DataTable()
            let adapter1 = new MySqlDataAdapter()
            adapter1.SelectCommand <- cmd1
            adapter1.Fill(dt1) |> ignore
            if dt1.Rows.Count > 0 then
                let updateTender =
                    sprintf "UPDATE %stender SET num_version = @num_version WHERE id_tender = @id_tender" stn.Pref
                for ten in dt1.Rows do
                    let idTender = (ten.["id_tender"] :?> int)
                    let cmd2 = new MySqlCommand(updateTender, con)
                    cmd2.Prepare()
                    cmd2.Parameters.AddWithValue("@id_tender", idTender) |> ignore
                    cmd2.Parameters.AddWithValue("@num_version", !verNum) |> ignore
                    cmd2.ExecuteNonQuery() |> ignore
                    incr verNum
            ()
    
    member this.TenderKwords (con: MySqlConnection) (idTender: int) (stn: S.T): unit =
        let resString = new StringBuilder()
        let selectPurObj =
            sprintf
                "SELECT DISTINCT po.name, po.okpd_name FROM %spurchase_object AS po LEFT JOIN %slot AS l ON l.id_lot = po.id_lot WHERE l.id_tender = @id_tender"
                stn.Pref stn.Pref
        let cmd1 = new MySqlCommand(selectPurObj, con)
        cmd1.Prepare()
        cmd1.Parameters.AddWithValue("@id_tender", idTender) |> ignore
        let dt = new DataTable()
        let adapter = new MySqlDataAdapter()
        adapter.SelectCommand <- cmd1
        adapter.Fill(dt) |> ignore
        if dt.Rows.Count > 0 then
            let distrDt = dt.Rows
            for row in distrDt do
                let name =
                    match row.IsNull("name") with
                    | true -> ""
                    | false -> string <| row.["name"]

                let okpdName =
                    match row.IsNull("okpd_name") with
                    | true -> ""
                    | false -> string <| row.["okpd_name"]

                resString.Append(sprintf "%s %s " name okpdName) |> ignore
        let selectAttach = sprintf "SELECT DISTINCT file_name FROM %sattachment WHERE id_tender = @id_tender" stn.Pref
        let cmd2 = new MySqlCommand(selectAttach, con)
        cmd2.Prepare()
        cmd2.Parameters.AddWithValue("@id_tender", idTender) |> ignore
        let dt2 = new DataTable()
        let adapter2 = new MySqlDataAdapter()
        adapter2.SelectCommand <- cmd2
        adapter2.Fill(dt2) |> ignore
        if dt2.Rows.Count > 0 then
            let distrDt = dt2.Rows
            for row in distrDt do
                let attName =
                    match row.IsNull("file_name") with
                    | true -> ""
                    | false -> string <| row.["file_name"]
                resString.Append(sprintf " %s" attName) |> ignore
        let idOrg = ref 0
        let selectPurInf =
            sprintf "SELECT purchase_object_info, id_organizer, extend_scoring_date, extend_bidding_date FROM %stender WHERE id_tender = @id_tender" stn.Pref
        let cmd3 = new MySqlCommand(selectPurInf, con)
        cmd3.Prepare()
        cmd3.Parameters.AddWithValue("@id_tender", idTender) |> ignore
        let dt3 = new DataTable()
        let adapter3 = new MySqlDataAdapter()
        adapter3.SelectCommand <- cmd3
        adapter3.Fill(dt3) |> ignore
        if dt3.Rows.Count > 0 then
            for row in dt3.Rows do
                let purOb =
                    match row.IsNull("purchase_object_info") with
                    | true -> ""
                    | false -> string <| row.["purchase_object_info"]
                let extScor =
                    match row.IsNull("extend_scoring_date") with
                    | true -> ""
                    | false -> string <| row.["extend_scoring_date"]
                let extBind =
                    match row.IsNull("extend_bidding_date") with
                    | true -> ""
                    | false -> string <| row.["extend_bidding_date"]
                idOrg := match row.IsNull("id_organizer") with
                         | true -> 0
                         | false -> row.["id_organizer"] :?> int
                resString.Append(sprintf " %s %s %s" purOb extScor extBind) |> ignore
        match (!idOrg) <> 0 with
        | true ->
            let selectOrg =
                sprintf "SELECT full_name, inn FROM %sorganizer WHERE id_organizer = @id_organizer" stn.Pref
            let cmd4 = new MySqlCommand(selectOrg, con)
            cmd4.Prepare()
            cmd4.Parameters.AddWithValue("@id_organizer", !idOrg) |> ignore
            let dt4 = new DataTable()
            let adapter4 = new MySqlDataAdapter()
            adapter4.SelectCommand <- cmd4
            adapter4.Fill(dt4) |> ignore
            if dt4.Rows.Count > 0 then
                for row in dt4.Rows do
                    let innOrg =
                        match row.IsNull("inn") with
                        | true -> ""
                        | false -> string <| row.["inn"]

                    let nameOrg =
                        match row.IsNull("full_name") with
                        | true -> ""
                        | false -> string <| row.["full_name"]

                    resString.Append(sprintf " %s %s" innOrg nameOrg) |> ignore
        | false -> ()
        let selectCustomer =
            sprintf
                "SELECT DISTINCT cus.inn, cus.full_name FROM %scustomer AS cus LEFT JOIN %spurchase_object AS po ON cus.id_customer = po.id_customer LEFT JOIN %slot AS l ON l.id_lot = po.id_lot WHERE l.id_tender = @id_tender"
                stn.Pref stn.Pref stn.Pref
        let cmd6 = new MySqlCommand(selectCustomer, con)
        cmd6.Prepare()
        cmd6.Parameters.AddWithValue("@id_tender", idTender) |> ignore
        let dt5 = new DataTable()
        let adapter5 = new MySqlDataAdapter()
        adapter5.SelectCommand <- cmd6
        adapter5.Fill(dt5) |> ignore
        if dt5.Rows.Count > 0 then
            let distrDt = dt5.Rows
            for row in distrDt do
                let innC =
                    match row.IsNull("inn") with
                    | true -> ""
                    | false -> string <| row.["inn"]

                let fullNameC =
                    match row.IsNull("full_name") with
                    | true -> ""
                    | false -> string <| row.["full_name"]

                resString.Append(sprintf " %s %s" innC fullNameC) |> ignore
        let mutable resS = Regex.Replace(resString.ToString(), @"\s+", " ")
        resS <- (resS.Trim())
        let updateTender =
            sprintf "UPDATE %stender SET tender_kwords = @tender_kwords WHERE id_tender = @id_tender" stn.Pref
        let cmd5 = new MySqlCommand(updateTender, con)
        cmd5.Prepare()
        cmd5.Parameters.AddWithValue("@id_tender", idTender) |> ignore
        cmd5.Parameters.AddWithValue("@tender_kwords", resS) |> ignore
        let res = cmd5.ExecuteNonQuery()
        if res <> 1 then Logging.Log.logger ("Не удалось обновить tender_kwords", idTender)
        ()
    
    member internal __.SetCancelStatus(con: MySqlConnection, dateUpd: DateTime, purNum: string) =
        let mutable cancelStatus = 0
        let mutable updated = false
        let selectDateT = sprintf "SELECT id_tender, date_version, cancel FROM %stender WHERE purchase_number = @purchase_number AND type_fz = @type_fz" S.Settings.Pref
        let cmd2 = new MySqlCommand(selectDateT, con)
        cmd2.Prepare()
        cmd2.Parameters.AddWithValue("@purchase_number", purNum) |> ignore
        cmd2.Parameters.AddWithValue("@type_fz", __.typeFz) |> ignore
        let adapter = new MySqlDataAdapter()
        adapter.SelectCommand <- cmd2
        let dt = new DataTable()
        adapter.Fill(dt) |> ignore
        for row in dt.Rows do
            updated <- true
            match dateUpd >= ((row.["date_version"]) :?> DateTime) with
            | true -> row.["cancel"] <- 1
            | false -> cancelStatus <- 1

        let commandBuilder = new MySqlCommandBuilder(adapter)
        commandBuilder.ConflictOption <- ConflictOption.OverwriteChanges
        adapter.Update(dt) |> ignore
        (cancelStatus, updated)