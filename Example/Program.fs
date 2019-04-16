// Learn more about F# at http://fsharp.org

open Hopac
open Hopac.Infixes
open ModbusTypes
open System

[<EntryPoint>]
let main argv =
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
    let b = x.Offset |> int
    let c = x.Quantity |> int
    let e = b + c - 1
    let values : UInt16 list = [b..e] |> List.map (fun x -> Map.find x hReg )
    {
      Values = values
    }

  let writeRegFunc (x : WriteRegRequest) : WriteRegResponse = 
    let o = x.Offset |> int

    hReg
    |> Map.add o x.Value
    |> fun y -> hReg <- y

    {
      Offset = x.Offset
      Value = hReg |> Map.find o
    }

  let writeRegsFunc (x : WriteRegsRequest) : ResOffQuant = 
     let o = x.Offset |> int
     let c = x.Quantity |> int
     let vals = x.Values
     [0..(c-1)]
     |> List.map(fun x -> hReg |> Map.add (o + x) (vals |> List.item x) |> fun x -> hReg <- x)
     |> ignore
     {
       Quantity = x.Quantity
       Offset = x.Offset
     }

  let readDOFunc (x : ReqOffQuant) : ResBools = 
    let offset = x.Offset |> int
    let count = x.Quantity |> int
    let ``end`` = offset + count - 1
    let values : bool list = [offset..``end``] |> List.map (fun x -> Map.find x dos)
    {
      Status = values
    }

  let writeDOFunc (x : WriteDoRequest) : WriteDoResponse = 
    let start = x.Offset |> int
    dos 
    |> Map.add start x.Value
    |> fun x -> dos <- x      
    {      
      Offset = x.Offset       
      Value = dos |> Map.find start
    }

  let writeDOsFunc (x : WriteDosRequest) : ResOffQuant = 
    let offset = x.Offset |> int
    let qty = x.Quantity |> int
    let vals = x.Values
    [0..(qty-1)] 
    |> List.map(fun x -> dos |> Map.add (offset + x) (vals |> List.item x) |> fun x -> dos <- x) // hacky!
    |> ignore
    {
      Offset = x.Offset
      Quantity = x.Quantity
    }

  let actionFunc : ModFunc = 
    let r (req : Request) : Response =
      match req with 
      | ReadDOReq x -> readDOFunc x |> ReadDORes
      | ReadDIReq x -> ModFunc.Default.Success.readDIFunc x 
      | ReadHRegReq x -> readHRegFunc x |> ReadHRegRes
      | ReadIRegReq x -> ModFunc.Default.Success.readIRegFunc x 
      | WriteDOReq x -> writeDOFunc x |> WriteDORes
      | WriteRegReq x -> writeRegFunc x |> WriteRegRes
      | WriteDOsReq x -> writeDOsFunc x |> WriteDOsRes
      | WriteRegsReq x -> ModFunc.Default.Success.writeRegsFunc x
    r

  let conf = 
    conf |> function | Ok x -> x // okay to crash here
  
  let modServer = Modbus.server conf actionFunc

  let app = 
    match runApp, shouldTest with 
    | true, _ -> 
      modServer >>- (fun x -> 0)
    | false, false -> 
      modServer >>- (fun x -> 0)
    | _ -> 
      0 |> Job.result

  let tests =
    match shouldTest with
    | true -> 
      Tests.runTests
    | false -> 
      0 |> Job.result

  [app; tests] 
  |> Job.conCollect
  |> Hopac.run 
  |> Seq.tryFind (fun x -> x <> 0) 
  |> Option.defaultValue 0
