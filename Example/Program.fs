// Learn more about F# at http://fsharp.org
open ModbusTypes
open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Console

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

  // pull out the conf
  let conf = conf |> function | Ok conf -> conf | _ -> exn "invalid conf" |> raise
  let logger = 
    LoggerFactory.Create(fun o -> 
      o.AddConsole() |> ignore
    ).CreateLogger()
  // build the hopac server
  Modbus.Server.build logger conf actionFunc |> Async.RunSynchronously
  0