
module LoggingTypes 
open System
open Hopac

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

// uses 'ticks', this is number of nanoseconds since the unix epoc
type DateTime' = uint64
module DateTime' = 
  
  let private msEpoc () = 
    DateTime(1970,1,1,0,0,0,0,DateTimeKind.Utc)

  let private msDtNow () = DateTime.UtcNow
  
  let FromMsDt (msdt : DateTime) : DateTime' = 
    let diff = msdt - msEpoc()
    (diff.Ticks |> uint64 ) * 100UL // convert to nano

  let Now () = 
    FromMsDt (msDtNow())

  let ToISO (dt : DateTime') =
    let t = dt / 100UL |> int64 // convert to Ticks
    let ts = TimeSpan.FromTicks(t)
    let msdt = (msEpoc()) + ts
    msdt.ToString("o")

type Message = 
  {
    LogLevel : LogLevel
    Message : string
    Tags : string list
    Fields : Map<string, obj>
    DateTime : DateTime'
  }
  static member Simple level message = 
    {
      LogLevel = level
      Message = message
      Tags = []
      Fields = Map.empty
      DateTime = (DateTime'.Now())
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

