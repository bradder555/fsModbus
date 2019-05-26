module Test.Client

open Expecto

open System
open System.Net
open System.Net.Sockets

open Hopac
open Hopac.Infixes

open ModbusTypes
open GracefulShutdown

open FsLoggingTypes
open System.Net.NetworkInformation


// get a free port for testing purposes
let getFreePort () = 
  job {
    let mapIps (x : IPEndPoint seq )= 
      x 
      |> Seq.toList
      |> List.map (fun x -> x.Port)

    let ipProps = 
      IPGlobalProperties
        .GetIPGlobalProperties()

    let tcpConnections = 
      ipProps
        .GetActiveTcpConnections()
      |> Seq.map (fun x -> x.LocalEndPoint)
      |> mapIps

    let tcpListeners = 
      ipProps
        .GetActiveTcpListeners()
      |> mapIps

    let udpListeners = 
      ipProps
        .GetActiveUdpListeners()
      |> mapIps

    let usedPorts =
      tcpConnections @ tcpListeners @ udpListeners
      |> List.fold (fun a n -> a |> Map.add n () ) Map.empty
      |> Map.toList
      |> List.map fst

    let ourPortRange = [1025 .. 30000]

    return 
      ourPortRange 
      |> List.except usedPorts
      |> List.head
  }

let port = getFreePort () |> Hopac.run

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


let consoleLogger = FsLogging.ConsoleEndpoint.build () |> Hopac.run
let logger = 
  Logger.New()
  |> Logger.Add "verboseConsole" consoleLogger

let tests =
  testList "client" [
    
    test "DOs roundtrip" {
      let testData = [true; true; true; false; false; true]
      let address = 2 |> Convert.ToUInt16
      let outRes = IVar()

      let gracefulShutdown = GracefulShutdown.Build()
      let server = Modbus.Server.build logger serverConf actionFunc

      let actions =
        job {
          let! modClient = Modbus.Client.build logger clientConf
          // write values to DOs
          
          let ivar = IVar()
          do! Ch.give modClient.WriteDOs ((address, testData), ivar)
          let! res1 = ivar |> IVar.read
          let res1 = 
            match res1 with
            | Error e -> raise e
            | Ok r -> r

          // read values from DOs
          let ivar = IVar()
          let rdo : ReqOffQuant = {
            Address = address
            Quantity = 6 |> Convert.ToUInt16
          }
          do! Ch.give modClient.ReadDOs (rdo, ivar)
          let! res2 = ivar |> IVar.read
          let res2 = 
            match res2 with
            | Error e -> raise e
            | Ok r -> r 

          do! res2 |> IVar.fill outRes
        } |> Promise.queue |> Alt.prepare

      let actions = 
        Alt.withNackJob <| fun nack -> 
          Job.start( nack >>- (fun () -> (printfn "shutting down client" ))) >>-. actions
      
      Alt.choose [
        gracefulShutdown.Alt
        server
        actions
      ] |> Hopac.run

      let outRes = outRes |> IVar.read |> Hopac.run

      Expecto.Expect.equal outRes testData  "The result should equal the test data"
    }
    
  ]