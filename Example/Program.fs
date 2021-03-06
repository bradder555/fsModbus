﻿// Learn more about F# at http://fsharp.org
open ModbusTypes
open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Console
open Util

let writeRegFunc (reg : byref<Map<int, _>>) (x : WriteRegRequest) : WriteRegResponse =
  let o = x.Address |> int
  reg <- reg |> Map.add o x.Value

  {
    Address = x.Address
    Value = reg |> Map.find o
  }

// create the delegates that read and write to the datastore (or just pass-through)
let readRegFunc (reg : Map<int, _>) (x : ReqOffQuant) : ResRegs =
  let b = x.Address |> int
  let c = x.Quantity |> int
  let e = b + c - 1
  let values : UInt16 list = [b..e] |> List.map (fun x -> Map.find x reg )
  {
    Values = values
  }

let writeRegsFunc (reg : byref<Map<int, _>>) (x : WriteRegsRequest) : ResOffQuant =
   let o = x.Address |> int
   let c = x.Values |> List.length
   let vals = x.Values
   for x in [0..(c-1)] do
     reg <- reg |> Map.add (o + x) (vals |> List.item x)
   {
     Quantity = c |> uint16
     Address = x.Address
   }

let readDigFunc  (reg : Map<int, _>) (x : ReqOffQuant) : ResBools =
  let offset = x.Address |> int
  let count = x.Quantity |> int
  let ``end`` = offset + count - 1
  let values : bool list = [offset..``end``] |> List.map (fun x -> Map.find x reg)
  {
    Status = values
  }

let writeDoFunc  (reg : byref<Map<int, _>>) (x : WriteDoRequest) : WriteDoResponse =
  let start = x.Address |> int
  reg <- reg |> Map.add start x.Value
  {
    Address = x.Address
    Value = reg |> Map.find start
  }

let writeDOsFunc (reg : byref<Map<int, _>>) (x : WriteDosRequest) : ResOffQuant =
  let offset = x.Address |> int
  let qty = x.Values |> List.length
  let vals = x.Values
  for x in [0..(qty-1)] do
    reg <- reg |> Map.add (offset + x) (vals |> List.item x)
  {
    Address = x.Address
    Quantity = qty |> uint16
  }

let randomReg (reg : byref<Map<int, _>>) : unit = 
  let regsize = reg.Count - 1
  let rand = new System.Random()
  let i = rand.Next(0, regsize)
  let r = rand.Next(0, 65530) |> uint16
  reg <- reg |> Map.add i r

let randomI (reg : byref<Map<int, bool>>) : unit = 
  let regsize = reg.Count - 1
  let rand = new System.Random()
  let i = rand.Next(0, regsize)
  let newv = reg |> Map.find i |> not
  reg <- reg |> Map.add i newv

type ParsedArg = 
  | Missing 
  | Present
  | Value of string

module ParsedArg = 
  let flagOnly =
    function 
      | Present -> true
      | Value "true" -> true
      | _ -> false

let parseArgs (argv : seq<string>) key = 
  argv 
  |> Seq.tryFind (fun x -> x.ToLower().Contains(sprintf "--%s" key))
  |> function 
     | None -> Missing
     | Some x -> 
       match x.Contains('=') with 
       | true -> x.Split('=') |> Seq.tail |> Seq.reduce (fun a n -> sprintf "%s=%s" a n) |> ParsedArg.Value 
       | _ -> Present

module Result = 
  let ToOption = function | Ok x -> Some x | _ -> None

[<EntryPoint>]
let main argv =

  let argParser = parseArgs argv

  match argParser "help" with 
  | Missing -> ()
  | _ -> 
    printfn """
    Usage:
      --help Show this message
      --binding="tcp://127.0.0.1:502" Binding for Server and Client
      --run-server Runs the Server Component
      --run-randomizer Runs the Server Randomizer Component (not much use without the server component)
      --run-client Runs the Client Component
    """
    System.Environment.Exit(0)

  let serverConf = 
    argParser "binding" 
    |> function 
       | Value x ->
         x |> (ModbusTypes.ModbusServerConf.TryParse >> Result.ToOption >> Option.get)
       | _ -> {
           Port = 5502u
           IPAddress = Net.IPAddress.Parse("127.0.0.1")
         }

  let runServer = 
    argParser "run-server" |> ParsedArg.flagOnly

  let runClient = 
    argParser "run-client" |> ParsedArg.flagOnly

  let runRandomizers = 
    argParser "run-randomizer" |> ParsedArg.flagOnly

  // create the datastore
  let mutable dis = [0..100] |> List.map(fun x -> x, false) |> Map.ofList
  let mutable dos = [0..100] |> List.map(fun x -> x, false) |> Map.ofList
  let mutable hReg = [0..100] |> List.map(fun x -> x, 0us) |> Map.ofList
  let mutable iReg = [0..100] |> List.map(fun x -> x, 0us) |> Map.ofList

  // create the 'rtu action func' which takes the RtuRequest and returns the RtuResponse
  let actionFunc : ModFunc =
    let r (req : RtuRequest) : RtuResponse =
      match req with
      | ReadDOReq x -> readDigFunc dos x |> ReadDORes
      | ReadDIReq x -> readDigFunc dis x |> ReadDIRes
      | ReadHRegReq x -> readRegFunc hReg x |> ReadHRegRes
      | ReadIRegReq x -> readRegFunc iReg x |> ReadIRegRes
      | WriteDOReq x -> writeDoFunc &dos x |> WriteDORes
      | WriteRegReq x -> writeRegFunc &hReg x |> WriteRegRes
      | WriteDOsReq x -> writeDOsFunc &dos x |> WriteDOsRes
      | WriteRegsReq x -> writeRegsFunc &hReg x |> WriteRegsRes
    r

  // wrap up the action func in the MabpFunc, this takes the MBapRequest and returns the MBapResponse
  let actionFunc (unitNo : uint16) : MbapFunc =
    let r (req : MbapReq ) : Result<MbapRes, unit> =
      match req.UnitIdentifier with
      | unitNo ->
        // we're only interested in one unit, which is '0',
        // although we can have as many units as we want
        let res =
          req.Request
          |> actionFunc

        {
          TransactionIdentifier = req.TransactionIdentifier
          ProtocolIdentifier = req.ProtocolIdentifier
          UnitIdentifier = req.UnitIdentifier
          Response = res
        } |> Ok
    r

  let logger = 
    LoggerFactory.Create(fun o -> 
      o.AddConsole() |> ignore
      o.AddFilter(fun _ -> true) |> ignore 
    ).CreateLogger()

  let clientConf = 
    {
      Server = serverConf.IPAddress |> IPAddress'
      Port = serverConf.Port
      SlaveId = 1
    }

  let client () = Modbus.Client.build logger clientConf
  let randDelay = fun _ -> async {return! Async.Sleep 100} 
  let randDelay = Async.bind randDelay
  let iregRandomizer = async {randomReg &iReg} |> randDelay |> Async.ForeverServer
  let iRandomizer = async {randomI &dis} |> randDelay |> Async.ForeverServer
  let iReader = 
    async {
      try 
        let! client = client ()
        let r = client.ReadIRegs {Address = 0us; Quantity = 100us}
        do!
          async {
            let! r = r
            logger.LogInformation("client read {result}", r)
            do! Async.Sleep 500
          } |> Async.ForeverServer
      with e -> 
        logger.LogWarning("client issue")
    } |> Async.ForeverServer // if it fails, start over

  let hReader = 
    async {
      try 
        let! client = client ()
        let r = client.ReadHRegs {Address = 0us; Quantity = 100us}
        do!
          async {
            let! r = r
            logger.LogInformation("client read {result}", r)
            do! Async.Sleep 500
          } |> Async.ForeverServer
      with e -> 
        logger.LogWarning("client issue")
    } |> Async.ForeverServer // if it fails, start over

  let diReader = 
    async {
      try 
        let! client = client ()
        let r = client.ReadDIs {Address = 0us; Quantity = 100us}
        do!
          async {
            let! r = r
            logger.LogInformation("client read {result}", r)
            do! Async.Sleep 500
          } |> Async.ForeverServer
      with e -> 
        logger.LogWarning("client issue")
    } |> Async.ForeverServer // if it fails, start over

  let jobs = 
    match runServer with 
    | true -> [Modbus.Server.build logger serverConf (actionFunc 1us)]
    | _ -> []

  let jobs = 
    match runRandomizers with
    | true ->
      iregRandomizer :: iRandomizer :: jobs
    | _ -> jobs

  let jobs = 
    match runClient with 
    | true -> 
      iReader :: hReader :: diReader :: jobs
    | _ -> jobs 

  jobs 
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore
  0