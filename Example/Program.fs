// Learn more about F# at http://fsharp.org

open Hopac
open Hopac.Infixes
open ModbusTypes
open System

[<EntryPoint>]
let main argv =
  // will probably move testing into a separate project
  let shouldTest = argv |> Seq.tryFind (fun x -> x.ToLower() = "test") |> Option.isSome
  let runApp = argv |> Seq.tryFind (fun x -> x.ToLower() = "run") |> Option.isSome

  let conf =
    argv
    |> Seq.tryFind (fun x -> x.ToLower().Contains("binding"))
    |> Option.map (fun x -> x.Split('=') |> Seq.last)
    |> Option.defaultValue "tcp://127.0.0.1:5502"
    |> ModbusTypes.ModbusServerConf.TryParse

  let mutable dis = [0..100] |> List.map(fun x -> x, false) |> Map.ofList
  let mutable dos = [0..100] |> List.map(fun x -> x, false) |> Map.ofList
  let mutable hReg = [0..100] |> List.map(fun x -> x, 0us) |> Map.ofList
  let mutable iReg = [0..100] |> List.map(fun x -> x, 0us) |> Map.ofList

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
     let c = x.Quantity |> int
     let vals = x.Values
     [0..(c-1)]
     |> List.map(fun x -> hReg |> Map.add (o + x) (vals |> List.item x) |> fun x -> hReg <- x)
     |> ignore
     {
       Quantity = x.Quantity
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
    let qty = x.Quantity |> int
    let vals = x.Values
    [0..(qty-1)]
    |> List.map(fun x -> dos |> Map.add (offset + x) (vals |> List.item x) |> fun x -> dos <- x) // hacky!
    |> ignore
    {
      Address = x.Address
      Quantity = x.Quantity
    }

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

  // this is a bit hokey but can't think of an alternative
  let finished = IVar()

  let gracefulShutdown : Alt<unit> =

    fun nack -> job {
        let c = IVar()
        do!
          job {
            Console.ReadLine() |> ignore
            printfn "enter pressed"
            do! IVar.tryFill c ()
          } |> Job.queueIgnore


        do!
          job {
            AppDomain.CurrentDomain.ProcessExit.Add (
              fun e ->
                () |> IVar.tryFill c |> run
                IVar.read finished |> run // block here to allow the application to close
                ()
            )

          }
        // i don't like it, but Cntl+c doesn't work on dotnet core
        // this is because it kills the dotnet process, not our process
        (*
        Console.CancelKeyPress.Add (
          fun e ->
          printfn "control + c pressed"
          e.Cancel <- true
          exitEvent.Set() |> ignore
          () |> IVar.tryFill c |> start
        )*)
        return IVar.read c ^-> fun _ -> printfn "gracefully shutting down"
    } |> Alt.withNackJob

  let conf = conf |> function | Ok conf -> conf
  let server = Modbus.Server.build conf actionFunc
  job {
    do! Alt.choose [
      gracefulShutdown
      server
    ]
  } |> Hopac.run

  // horrible global to tell processexit that the app has finished gracefully
  IVar.tryFill finished () |> start
  0
