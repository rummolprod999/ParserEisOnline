namespace ParserEis

open System
open System.IO
open System.Reflection
open FSharp.Data

module rec Stn =
    type TypeSettings =
        | St of T
        | None

    let Settings = ref TypeSettings.None

    type Sample = JsonProvider<"""{
  "database": "none",
  "prefix": "prefix",
  "userdb": "user",
  "passdb": "irhjdklfhj56456",
  "server": "localhost",
  "port": 3306
}""">
    type T =
        { TempPath: string
          Prefix: string
          ConStr: string }

    type U =
        { User: string
          Pass: string }

    let PathProgram: string =
        let path = Path.GetDirectoryName(Assembly.GetExecutingAssembly().GetName().CodeBase)
        if path <> null then path.Substring(5)
        else failwith "bad get programm path"

    let getSettings() =
        let pathSet = sprintf "%s%csettings.json" PathProgram Path.DirectorySeparatorChar
        use sr = new StreamReader(pathSet, System.Text.Encoding.Default)
        let s = Sample.Parse(sr.ReadToEnd())
        let Prefix = s.Prefix
        let connectstring = sprintf "server=%s;port=%d;database=%s;user=%s;password=%s;CharSet=utf8;Convert Zero Datetime=True;default command timeout=3600;Connection Timeout=3600;SslMode=none" s.Server s.Port s.Database s.Userdb s.Passdb
        let (_, tmpPath) = CreateDirs()
        Settings := St ({
                         TempPath = tmpPath
                         Prefix = s.Prefix
                         ConStr = connectstring
                         })
        let stn = match !Settings with
                  | Stn.None ->
                    Logging.Log.logger "bad settings"
                    failwith "bad settings"
                  | Stn.St x -> x
        S.Settings <- {
        TmpP = stn.TempPath
        Pref = stn.Prefix
        ConS = stn.ConStr
        }
        ()


    let CreateDirs() =
        let logPath = sprintf "%s%clogdir_%s" PathProgram Path.DirectorySeparatorChar <| S.argTuple.GetType().Name
        let tmpPath = sprintf "%s%ctempdir_%s" PathProgram Path.DirectorySeparatorChar <| S.argTuple.GetType().Name
        match Directory.Exists(tmpPath) with
        | true ->
                let dirInfo = new DirectoryInfo(tmpPath)
                dirInfo.Delete(true)
                Directory.CreateDirectory(tmpPath) |> ignore
        | false -> Directory.CreateDirectory(tmpPath) |> ignore
        match Directory.Exists(logPath) with
        | false -> Directory.CreateDirectory(logPath) |> ignore
        | true -> ()
        S.logFile <- sprintf "%s%clog_parsing_%s_%s.log" logPath Path.DirectorySeparatorChar <| S.argTuple.GetType().Name <| DateTime.Now.ToString("dd_MM_yyyy")
        (logPath, tmpPath)
