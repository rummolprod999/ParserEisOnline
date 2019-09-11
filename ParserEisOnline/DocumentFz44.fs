namespace ParserEis

open System.Data
open S
open Logging
open System
open MySql.Data.MySqlClient
open System

type DocumentFz44() =

      [<DefaultValue>] val mutable purNum: string
      [<DefaultValue>] val mutable status: string
      [<DefaultValue>] val mutable Url: string
      [<DefaultValue>] val mutable publishDate: DateTime
      [<DefaultValue>] val mutable updateDate: DateTime
      inherit AbstractDocument("ЕДИНАЯ ИНФОРМАЦИОННАЯ СИСТЕМА В СФЕРЕ ЗАКУПОК", "http://zakupki.gov.ru/", 44)
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
                           return ""
                       }
            match res with
                | Succ _ -> ()
                | Err e when e = "" -> ()
                | Err r -> Log.logger r
