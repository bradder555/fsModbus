# Introduction
I did not like the available modbus libraries, i figured 'how hard can
it be?' so i've decided to write it using fsharp and dotnet core

This has been developed on x86_64 Ubuntu, it should run on anything with 
a NIC and can run the dotnetcore 2.2 run time (so ARM, x86), (Windows, Raspbian..)

## Project Status
### Server
The following has been implemented and is working
- FC 1, Read Coils 
- FC 2, Read Inputs
- FC 3, Read Holding Registers 
- FC 5, Write a Single Coil
- FC 6, Write a Single Holding Register 
- FC 15, Write multiple coils
- FC 16, Write multiple holding registers 

### Client
- FC 1, Tested and confirmed working
- FC 6, Tested and confirmed working

There is no test coverage of the other function codes, best to assume nothing else is working.

## Usage
### Server
(there is an example project in the git)
```fsharp
// Learn more about F# at http://fsharp.org

open Hopac
open Hopac.Infixes
open ModbusTypes
open System
open GracefulShutdown
open FsLoggingTypes

[<EntryPoint>]
let main argv =

  let conf =
    argv
    |> Seq.tryFind (fun x -> x.ToLower().Contains("binding"))
    |> Option.map (fun x -> x.Split('=') |> Seq.last)
    |> Option.defaultValue "tcp://127.0.0.1:5502"
    |> ModbusTypes.ModbusServerConf.TryParse
  
  // create the datastore
  let mutable dis = [0..100] |> List.map(fun x -> x, false) |> Map.ofList
  let mutable dos = [0..100] |> List.map(fun x -> x, false) |> Map.ofList
  let mutable hReg = [0..100] |> List.map(fun x -> x, 0us) |> Map.ofList
  let mutable iReg = [0..100] |> List.map(fun x -> x, 0us) |> Map.ofList

  // create the delegates that read and write to the datastore (or just pass-through)
  let readHRegFunc (x : ReqOffQuant) : ResRegs =
    let b = x.Address |> int
    let c = x.Quantity |> int
    let e = b + c - 1
    let values : UInt16 list = [b..e] |> List.map (fun x -> Map.find x hReg )
    {
      Values = values
    }

  let writeRegFunc (x : WriteRegRequest) : WriteRegResponse =
    let o = x.Address |> int

    hReg
    |> Map.add o x.Value
    |> fun y -> hReg <- y

    {
      Address = x.Address
      Value = hReg |> Map.find o
    }

  let writeRegsFunc (x : WriteRegsRequest) : ResOffQuant =
     let o = x.Address |> int
     let c = x.Values |> List.length
     let vals = x.Values
     [0..(c-1)]
     |> List.map(fun x -> hReg |> Map.add (o + x) (vals |> List.item x) |> fun x -> hReg <- x)
     |> ignore
     {
       Quantity = c |> uint16
       Address = x.Address
     }

  let readDOFunc (x : ReqOffQuant) : ResBools =
    let offset = x.Address |> int
    let count = x.Quantity |> int
    let ``end`` = offset + count - 1
    let values : bool list = [offset..``end``] |> List.map (fun x -> Map.find x dos)
    {
      Status = values
    }

  let writeDOFunc (x : WriteDoRequest) : WriteDoResponse =
    let start = x.Address |> int
    dos
    |> Map.add start x.Value
    |> fun x -> dos <- x
    {
      Address = x.Address
      Value = dos |> Map.find start
    }

  let writeDOsFunc (x : WriteDosRequest) : ResOffQuant =
    let offset = x.Address |> int
    let qty = x.Values |> List.length
    let vals = x.Values
    [0..(qty-1)]
    |> List.map(fun x -> dos |> Map.add (offset + x) (vals |> List.item x) |> fun x -> dos <- x) // hacky!
    |> ignore
    {
      Address = x.Address
      Quantity = qty |> uint16
    }

  // create the 'rtu action func' which takes the RtuRequest and returns the RtuResponse
  let actionFunc : ModFunc =
    let r (req : RtuRequest) : RtuResponse =
      match req with
      | ReadDOReq x -> readDOFunc x |> ReadDORes
      | ReadDIReq x -> ModFunc.Default.Success.readDIFunc x
      | ReadHRegReq x -> readHRegFunc x |> ReadHRegRes
      | ReadIRegReq x -> ModFunc.Default.Success.readIRegFunc x
      | WriteDOReq x -> writeDOFunc x |> WriteDORes
      | WriteRegReq x -> writeRegFunc x |> WriteRegRes
      | WriteDOsReq x -> writeDOsFunc x |> WriteDOsRes
      | WriteRegsReq x -> writeRegsFunc x |> WriteRegsRes
    r

  // wrap up the action func in the MabpFunc, this takes the MBapRequest and returns the MBapResponse
  let actionFunc : MbapFunc =
    let r (req : MbapReq ) : Result<MbapRes, unit> =
      match req.UnitIdentifier with
      | 0uy ->
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
      | _ -> () |> Error
    r

  // build the graceful shutdown alternative
  let gracefulShutdown = GracefulShutdown.Build()

  // pull out the conf
  let conf = conf |> function | Ok conf -> conf | _ -> exn "invalid conf" |> raise
  
  // build the console logger for the logger
  let consoleLogger = FsLogging.ConsoleEndpoint.build () |> Hopac.run
  
  // create a new logger and attach the console logger to it
  let logger =
    Logger.New()
    |> Logger.Add "verboseConsole" consoleLogger

  // build the hopac server
  let server = Modbus.Server.build logger conf actionFunc
  
  // run the server, it should block until one of the alts is committed to
  job {
    do! Alt.choose [
      gracefulShutdown.Alt
      server
    ]
  } |> Hopac.run

  // if the previously blocked job is over, make sure the gracefulShutdown is finished, so the application exits
  gracefulShutdown.Finished()
  0


```

## Testing Status
A heap of utility functions were written to assist with all of the bit,
byte and word manipulations. 

The utility functions have ~100% test coverage - all of this is fairly
trivial, but it gets confusing fast! better to test, so behaviour is
documented also

### Server
The server had almost 100% test coverage, at one point, a major change was made that broke the tests.
It was not a major loss, since the functionality had been tested prior, and the change made wouldn't have changed this.
I'd like to restore this in the future for the sake of completeness

### Client
The client is tested against the server for complete round-trip testing.

A nice feature of the code, is the user of the server has two write their own delegate actions for each of the function codes. Consequently, when initializing the server we provide our own functions, this means that we have access to the mutable store that is used by the server.

This hasn't been used yet, in the client testing, but it's certainly handy.

## Philosophy / Design decisions
It can get a bit confusing, getting one's head around byte orders,
what i have adopted in my code is, the head of the list should be the
least-significant, i.e. a uint of 258 should be converted to [2uy; 1uy]

What is important is making the decision and sticking with it

## Todo:
- handle out-of-bounds and other deserialization exceptions
- develop and test the handling of receiving 'junk' (leads on from the first point)
- handle any other modbus-spec exceptions
- write a client interface
- add tests for
  - and exception handling
- add round-trip testing
