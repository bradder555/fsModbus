module Test.Client

open Expecto

open System
open System.Net
open System.Net.Sockets

open Hopac

open ModbusTypes

(* -- the following doesn't work -- replaced by hack
// get a free port for testing purposes
let t = TcpListener(IPAddress.Loopback, 0)
t.Start()
let port = (t.LocalEndpoint :?> IPEndPoint).Port
t.Stop()
*)

let port = 30114

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

let actionFunc' : ModFunc =
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
          |> actionFunc'

        {
          TransactionIdentifier = req.TransactionIdentifier
          ProtocolIdentifier = req.ProtocolIdentifier
          UnitIdentifier = req.UnitIdentifier
          Response = res
        } |> Ok
      | _ -> () |> Error
    r

// create a server for testing
let serverConf = {
    Port = port |> Convert.ToUInt32
    IPAddress = IPAddress.Loopback
}

let clientConf : ModbusTypes.ModbusClientConf =
  {
    Port = port |> Convert.ToUInt32
    Server = IPAddress.Loopback |> IPAddress'
    SlaveId = 0
  }

let tests =
  testList "client" [
    (*
    test "DOs roundtrip" {
      let testData = [true; true; true; false; false; true]
      let res =
        job {
          // set up the server
          let cancelServerI : IVar<unit> = IVar()
          let cancelServer = cancelServerI |> IVar.read
          do! Modbus.Server.build serverConf actionFunc

          // set up the client
          let cancelClientI : IVar<unit> = IVar()
          let cancelClient = cancelClientI |> IVar.read
          let! modClient = Modbus.Client.build clientConf

          // write values to DOs
          let offset = 2 |> Convert.ToUInt16
          let ivar = IVar()
          do! Ch.give modClient.WriteDOs ((offset, testData), ivar)
          let! res = ivar |> IVar.read
          match res with
          | Error e -> raise e
          | _ -> ignore

          // read values from DOs
          let ivar = IVar()
          let rdo : ReqOffQuant = {
            Address = 2 |> Convert.ToUInt16
            Quantity = 6 |> Convert.ToUInt16
          }
          do! Ch.give modClient.ReadDOs (rdo, ivar)
          let! res = ivar |> IVar.read
          match res with
          | Error e -> raise e
          | _ -> ignore

          let res = res |> function | Ok r -> r // okay to be incomplete

          do! IVar.fill cancelServerI ()
          do! IVar.fill cancelClientI ()
          return testData
        } |> Hopac.run

      Expecto.Expect.equal testData res "The result should equal the test data"
    }
    *)
  ]