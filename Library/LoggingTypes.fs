module LoggingTypes 
open System
open Hopac
open Hopac.Infixes
 
// this is number of nanoseconds since the unix epoc
// this should be it's own library
type DateTime' = | DateTime'' of uint64
module DateTime' = 
  let private unpack (dt : DateTime') : uint64 = 
    (dt |> function | DateTime'' x -> x) 

  let private pack (ui : uint64) : DateTime' = 
    ui |> DateTime''

  let private flip fn a b = fn b a
  let inline private (/.) a b = b / a
    
  let private msEpoc () = 
    DateTime(1970,1,1,0,0,0,0,DateTimeKind.Utc)
    
  let private msDtNow () = 
    job { return DateTime.UtcNow }
  
  let FromMsDt (msdt : DateTime) : DateTime' = 
    let diff = msdt - msEpoc()
    (diff.Ticks |> uint64 ) * 100UL |> DateTime'' // convert to nano

  let FromNano (u : uint64) = u |> pack
  let FromMicro (u : uint64) = u * 1000UL |> pack
  let FromMilli (u : uint64) = u * 1000UL |> FromMicro
  let FromUnix (u : uint64) = u * 1000UL |> FromMilli

  let Now () = 
    (msDtNow ()) >>- FromMsDt

  let ToISO (dt : DateTime') =
    let t = 
      unpack dt 
      |> (/.) 100UL 
      |> int64  // convert to Ticks

    let ts = TimeSpan.FromTicks(t)
    let msdt = (msEpoc()) + ts
    msdt.ToString("o")

  let ToMicro (dt : DateTime') = 
    unpack dt 
    |> (/.) 1000UL // nano -> micro

  let ToMilli (dt : DateTime') = 
    dt 
    |> ToMicro
    |> (/.) 1000UL

  let ToUnix (dt : DateTime') = 
    dt 
    |> ToMilli
    |> (/.) 1000UL // micro -> seconds

  let TryParse (s : string) = 
    match DateTime.TryParse(s) with 
    | true, x -> Some x
    | false, _ -> None
    |> Option.map FromMsDt


// this should be it's own library
type LogLevel = 
  | Debug
  | Verbose
  | Information
  | Warning
  | Exception
with override x.ToString() =
      match x with
      | Debug -> "DEBUG"
      | Verbose -> "VERBOSE"
      | Information -> "INFO"
      | Warning -> "WARN"
      | Exception -> "EXCEPT"

type Message = 
  {
    LogLevel : LogLevel
    Message : string
    Tags : string list
    Fields : Map<string, obj>
    DateTime : DateTime'
  }
  static member Simple level message = 
    DateTime'.Now()
    >>- fun x -> 
      {
        LogLevel = level
        Message = message
        Tags = []
        Fields = Map.empty
        DateTime = x
      }
  static member AddField (key : string) (o : obj) (x : Message)= 
    {
      LogLevel = x.LogLevel
      Message = x.Message
      Tags = x.Tags 
      Fields = x.Fields |> Map.add key o
      DateTime = x.DateTime 
    }
  static member AddFields (fields : Map<string, obj>) (x : Message) = 
    let f1 = x.Fields |> Map.toList
    let f2 = fields |> Map.toList
    let f = f1 @ f2
    let m = f |> List.fold (fun m (s,o) -> m |> Map.add s o) Map.empty
    {
      LogLevel = x.LogLevel
      Message = x.Message 
      Tags = x.Tags 
      Fields = m
      DateTime = x.DateTime 
    }

type Endpoint = Message -> Job<unit>

type Endpoints = | Endpoints' of Map<string, Endpoint>

// This is my logger in its simplest form,
// you build a logger, you add endpoints to it,
// then you log, its up to the endpoint wether it should
// log depending on the verbosity
// The logger is injected everywhere we intend to use it
type Logger = 
  {
    Endpoints : Endpoints
  }
    static member Add (key : string) (func : Endpoint) (x : Logger) : Logger =
      let ep = x.Endpoints |> function | Endpoints' x -> x 
      let next = ep |> Map.add key func
      let next' = next |> Endpoints'  
      {
        Endpoints = next' 
      }
    member x.Remove (key : string) : Logger = 
      let ep = x.Endpoints |> function | Endpoints' x -> x
      let next = ep |> Map.remove key
      let next' = next |> Endpoints'
      {
        Endpoints = next' 
      }
    member x.Log (msg : Message ) = 
      let ep = x.Endpoints |> function | Endpoints' x -> x
      let funcs = ep |> Map.toList |> List.map snd
      funcs |> List.map (fun fn -> fn msg) |> Job.conIgnore

    static member New () : Logger = 
        {
          Endpoints = Map.empty |> Endpoints'
        }

