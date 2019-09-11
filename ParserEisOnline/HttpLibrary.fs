namespace ParserEis

open Logging
open System
open System.IO
open System.Net
open System.Net.Http
open System.Text
open System.Threading
open System.Threading.Tasks

module Download =
    type TimedWebClient() =
        inherit WebClient()
        override this.GetWebRequest(address: Uri) =
            let wr = base.GetWebRequest(address) :?> HttpWebRequest
            wr.Timeout <- 600000
            wr.UserAgent <- "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:55.0) Gecko/20100101 Firefox/55.0"
            wr :> WebRequest

    type TimedWebClientBot() =
            inherit WebClient()
            override this.GetWebRequest(address: Uri) =
                let wr = base.GetWebRequest(address) :?> HttpWebRequest
                wr.Timeout <- 600000
                wr.UserAgent <- "Mozilla/5.0 (compatible; YandexBot/3.0; +http://yandex.com/bots) Gecko/20100101 Firefox/55.0"
                wr :> WebRequest

    let DownloadString url =
        let mutable s = null
        let count = ref 0
        let mutable continueLooping = true
        while continueLooping do
            try
                //let t ():string = (new TimedWebClient()).DownloadString(url: Uri)
                let task = Task.Run(fun () -> (new TimedWebClient()).DownloadString(url: string))
                if task.Wait(TimeSpan.FromSeconds(100.)) then
                    s <- task.Result
                    continueLooping <- false
                else raise <| new TimeoutException()
            with
                | :? WebException as e when e.Response.GetType() = typedefof<HttpWebResponse> && (e.Response :?> HttpWebResponse).StatusCode = HttpStatusCode.Forbidden -> continueLooping <- false; Logging.Log.logger (sprintf "Forbidden %s" url)

                | _ when !count >= 10 ->
                            Logging.Log.logger (sprintf "Не удалось скачать %s за %d попыток" url !count)
                            continueLooping <- false
                | t when t.InnerException.Message.Contains("(404) Not Found") -> continueLooping <- false; Logging.Log.logger (sprintf "404 Page %s" url)
                | z when z.InnerException.Message.Contains("(403) Forbidden") -> continueLooping <- false; Logging.Log.logger (sprintf "403 Page %s" url)
                | p when p.InnerException.Message.Contains("The remote server returned an error: (434)") -> continueLooping <- false; Logging.Log.logger (sprintf "434 Page %s" url)
                | y -> incr count
                       Logging.Log.logger (sprintf "Error %s %s" url y.Message)
                       Thread.Sleep(5000)
        s

    let DownloadStringBot url =
        let mutable s = null
        let count = ref 0
        let mutable continueLooping = true
        while continueLooping do
            try
                //let t ():string = (new TimedWebClient()).DownloadString(url: Uri)
                let task = Task.Run(fun () -> (new TimedWebClientBot()).DownloadString(url: string))
                if task.Wait(TimeSpan.FromSeconds(100.)) then
                    s <- task.Result
                    continueLooping <- false
                else raise <| new TimeoutException()
            with _ ->
                if !count >= 3 then
                    Logging.Log.logger (sprintf "Не удалось скачать %s за %d попыток" url !count)
                    continueLooping <- false
                else incr count
                Thread.Sleep(5000)
        s

    let DownloadString1251 url =
        let mutable s = null
        let count = ref 0
        let mutable continueLooping = true

        let getWebClient() =
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
            let a = new TimedWebClient()
            a.Encoding <- Encoding.GetEncoding("windows-1251")
            a
        while continueLooping do
            try
                //let t ():string = (new TimedWebClient()).DownloadString(url: Uri)
                let task = Task.Run(fun () -> (getWebClient()).DownloadString(url: string))
                if task.Wait(TimeSpan.FromSeconds(650.)) then
                    s <- task.Result
                    continueLooping <- false
                else raise <| new TimeoutException()
            with _ ->
                if !count >= 3 then
                    Logging.Log.logger (sprintf "Не удалось скачать %s за %d попыток" url !count)
                    continueLooping <- false
                else incr count
                Thread.Sleep(5000)
        s

    let DownloadString1251Bot url =
            let mutable s = null
            let count = ref 0
            let mutable continueLooping = true

            let getWebClient() =
                Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
                let a = new TimedWebClientBot()
                a.Encoding <- Encoding.GetEncoding("windows-1251")
                a
            while continueLooping do
                try
                    //let t ():string = (new TimedWebClient()).DownloadString(url: Uri)
                    let task = Task.Run(fun () -> (getWebClient()).DownloadString(url: string))
                    if task.Wait(TimeSpan.FromSeconds(650.)) then
                        s <- task.Result
                        continueLooping <- false
                    else raise <| new TimeoutException()
                with _ ->
                    if !count >= 5 then
                        Logging.Log.logger (sprintf "Не удалось скачать %s за %d попыток" url !count)
                        continueLooping <- false
                    else incr count
                    Thread.Sleep(5000)
            s

    let DownloadFileSimple (url: string) (patharch: string): FileInfo =
        let mutable ret = null
        let downCount = ref 0
        let mutable cc = true
        while cc do
            try
                let wc = new WebClient()
                wc.DownloadFile(url, patharch)
                ret <- new FileInfo(patharch)
                cc <- false
            with _ ->
                let FileD = new FileInfo(patharch)
                if FileD.Exists then FileD.Delete()
                if !downCount = 0 then
                    Logging.Log.logger (sprintf "Не удалось скачать %s за %d попыток" url !downCount)
                    cc <- false
                else decr downCount
                Thread.Sleep(5000)
        ret
