module Modbus
// for my code, the head should always be the least significant

open ModbusTypes

open System
open System.Net
open System.Net.Sockets
open Util

open Microsoft.Extensions.Logging

let disposeSocket (logger : ILogger) (handle : Socket) =
  async {
    try
      let remote = handle.RemoteEndPoint :?> IPEndPoint
      logger.LogInformation(
        "{action} remote ip: {remote-ip} and port: {remote-port}", 
        remote.Address, 
        remote.Port, 
        "client-disconnect"
      )
    with | _ -> ()

    try
      handle.Close(0)
    with | _ -> ()

    try
      handle.Dispose()
    with | _ -> ()
  }
module Server =

  let build (logger : ILogger ) (conf : ModbusServerConf) (actionFunc : MbapFunc) : Async<unit> =
    logger.LogInformation(
      "Building the Modbus Server")

    let disposeSocket = disposeSocket logger
    // handle receive is only ever called from the server function
    // should not be in the external API, instead of using private, better
    // to scope it to this function exclusively
    let handleRec (handle : Socket) (inBuff : ArraySegment<byte>) (bufferLen : int) =
      async {
        let remote = handle.RemoteEndPoint :?> IPEndPoint
        try
          match handle.Connected && bufferLen > 0 with
            | true ->
              let frame = inBuff.Slice(0, bufferLen).ToArray() |> Array.toList
              let reqMbap = MbapReq.TryParse frame
              match reqMbap with
              | Ok r ->
                logger.LogDebug(
                  "{remote} {data}",
                    sprintf "%A:%A" remote.Address remote.Port,
                    r
                )

                let res = actionFunc r
                match res with
                | Error _ -> 
                  logger.LogWarning("error responding to request")
                | Ok res ->
                  logger.LogDebug(
                    "{remote} {data}",
                      sprintf "%A:%A" remote.Address remote.Port,
                      res
                  )

                  let buff =
                    res.Serialize()
                    |> List.toArray
                    |> ArraySegment

                  do! handle.SendAsync(buff, SocketFlags.None) |> Async.AwaitTask |> Async.Ignore
                  return ()
              | Error e ->
                logger.LogWarning("unable to parse request with exception {exception}", e)
                return! disposeSocket handle
            | false ->
              logger.LogWarning("not connected to client")
              return! disposeSocket handle
        with
        | _ ->
          return! disposeSocket handle
      }

    let rec handleReceive (handle : Socket) =
      async {
        let inBuff : ArraySegment<byte> = Array.zeroCreate(300) |> ArraySegment
        match handle.Connected with
        | true ->
          let! recLen = handle.ReceiveAsync(inBuff, SocketFlags.None) |> Async.AwaitTask 
          do! handleRec handle inBuff recLen
          do! handleReceive handle
        | false ->
          ()
      }

    let listener = new Socket(SocketType.Stream, ProtocolType.Tcp)
    listener.Bind(IPEndPoint(conf.IPAddress, int conf.Port))
    listener.Listen(100)
    let rec loop () = 
      async {
        let! handler = listener.AcceptAsync() |> Async.AwaitTask
        let remote = handler.RemoteEndPoint :?> IPEndPoint
        logger.LogInformation(
          "{action}: remote ip: {remote-ip} and port: {remote-port}",
          "client-connect",
          remote.Address,
          remote.Port
          )

        (handleReceive handler) |> Async.Start
        return! loop ()
      } 

    loop ()
    

module Client =
  let build (logger : ILogger) (conf : ModbusClientConf)  =
    let disposeSocket = disposeSocket logger
    let mutable transactionCounter = 0us
    let lock_obj = new obj()
     
    let client () =
      async {
        let c = new Socket(SocketType.Stream, ProtocolType.Tcp)
        let port = conf.Port |> int
        use connection =
          match conf.Server with
          | IPAddress' x ->
            c.ConnectAsync(x, port)
          | Hostname x ->
            c.ConnectAsync(x, port)
        do! connection |> Async.AwaitTask
        return c
      }
    
    async {
      let! client = client ()
      let readDis (initialReq : ReqOffQuant) : Async<Result<bool list, exn>> =
        lock lock_obj (fun () ->
          async{
            transactionCounter <- transactionCounter + 1us
            let transactionIdentifier = transactionCounter
            let req = {
              TransactionIdentifier = transactionIdentifier
              ProtocolIdentifier = 0us
              UnitIdentifier = conf.SlaveId |> byte
              Request = initialReq |> RtuRequest.ReadDIReq
            }
            let req = req.Serialize () |> List.toArray |> ArraySegment
            let! reqCount = client.SendAsync(req, SocketFlags.None) |> Async.AwaitTask
            let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
            let! resCount = client.ReceiveAsync(resBuff, SocketFlags.None) |> Async.AwaitTask
            // todo: resCount = 0 should yield an exception
            let resBuff = resBuff.Slice(0, resCount).ToArray() |> Array.toList

            let res = MbapRes.TryParse(resBuff)
            return
              match reqCount, resCount, res with
              | 0, _, _ ->
                "reqCount equal to zero" |> exn |> Error
              | _, 0, _ ->
                "resCount equal to zero" |> exn |> Error
              | _, _, Error (_, e) -> e |> Error
              | _, _, Ok x ->
                // do a heap of validation, i.e. TransactionIdentifier should equal the req TI
                if x.TransactionIdentifier <> transactionIdentifier then
                  "sprintf response is not to this request" |> exn |> Error
                else
                  match x.Response with
                  | ModErrorRes e ->
                    sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                    |> exn |> Error
                  | ReadDIRes x ->
                    x.Status |> Ok
                  | _ -> exn "unexpected return function code" |> Error
          }
        )

      let readHRegs (initialReq : ReqOffQuant) : Async<Result<uint16 list, exn>> =
        lock lock_obj (fun () ->
          async{
            transactionCounter <- transactionCounter + 1us
            let transactionIdentifier = transactionCounter
            let req = {
              TransactionIdentifier = transactionIdentifier
              ProtocolIdentifier = 0us
              UnitIdentifier = conf.SlaveId |> byte
              Request = initialReq |> RtuRequest.ReadHRegReq
            }
            let req = req.Serialize () |> List.toArray |> ArraySegment
            let! reqCount = client.SendAsync(req, SocketFlags.None) |> Async.AwaitTask
            let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
            let! resCount = client.ReceiveAsync(resBuff, SocketFlags.None) |> Async.AwaitTask
            // todo: resCount = 0 should yield an exception
            let resBuff = resBuff.Slice(0, resCount).ToArray() |> Array.toList

            let res = MbapRes.TryParse(resBuff)
            return
              match reqCount, resCount, res with
              | 0, _, _ ->
                "reqCount equal to zero" |> exn |> Error
              | _, 0, _ ->
                "resCount equal to zero" |> exn |> Error
              | _, _, Error (_, e) -> e |> Error
              | _, _, Ok x ->
                // do a heap of validation, i.e. TransactionIdentifier should equal the req TI
                if x.TransactionIdentifier <> transactionIdentifier then
                  "sprintf response is not to this request" |> exn |> Error
                else
                  match x.Response with
                  | ModErrorRes e ->
                    sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                    |> exn |> Error
                  | ReadHRegRes x ->
                    x.Values |> Ok
                  | _ -> exn "unexpected return function code" |> Error
          }
        )
        
      let readIRegs (initialReq : ReqOffQuant) : Async<Result<uint16 list, exn>> =
        lock lock_obj (fun () ->
          async{
            transactionCounter <- transactionCounter + 1us
            let transactionIdentifier = transactionCounter
            let req = {
              TransactionIdentifier = transactionIdentifier
              ProtocolIdentifier = 0us
              UnitIdentifier = conf.SlaveId |> byte
              Request = initialReq |> RtuRequest.ReadIRegReq
            }
            let req = req.Serialize () |> List.toArray |> ArraySegment
            let! reqCount = client.SendAsync(req, SocketFlags.None) |> Async.AwaitTask
            let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
            let! resCount = client.ReceiveAsync(resBuff, SocketFlags.None) |> Async.AwaitTask
            // todo: resCount = 0 should yield an exception
            let resBuff = resBuff.Slice(0, resCount).ToArray() |> Array.toList

            let res = MbapRes.TryParse(resBuff)
            return
              match reqCount, resCount, res with
              | 0, _, _ ->
                "reqCount equal to zero" |> exn |> Error
              | _, 0, _ ->
                "resCount equal to zero" |> exn |> Error
              | _, _, Error (_, e) -> e |> Error
              | _, _, Ok x ->
                // do a heap of validation, i.e. TransactionIdentifier should equal the req TI
                if x.TransactionIdentifier <> transactionIdentifier then
                  "sprintf response is not to this request" |> exn |> Error
                else
                  match x.Response with
                  | ModErrorRes e ->
                    sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                    |> exn |> Error
                  | ReadIRegRes x ->
                    x.Values |> Ok
                  | _ -> exn "unexpected return function code" |> Error
          }
        )


      let writeDos (offset, blist) =
        lock lock_obj ( fun () ->
          async{
            let req : WriteDosRequest =
              {
                Address = offset
                Values = blist
              }
         
            transactionCounter <- transactionCounter + 1us
            let transactionIdentifier = transactionCounter
            let req = {
              TransactionIdentifier = transactionIdentifier
              ProtocolIdentifier = 0us
              UnitIdentifier = conf.SlaveId |> byte
              Request = req |> RtuRequest.WriteDOsReq
            }

            let req = req.Serialize () |> List.toArray |> ArraySegment
            let! reqCount = client.SendAsync(req, SocketFlags.None) |> Async.AwaitTask

            let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
            let! resCount = client.ReceiveAsync(resBuff, SocketFlags.None) |> Async.AwaitTask
            // todo: resCount = 0 should yield an exception

            let resBuff = resBuff.Slice(0, resCount).ToArray() |> Array.toList

            let res = MbapRes.TryParse(resBuff)
            return
              match reqCount, resCount, res with
              | 0, _, _ ->
                "reqCount equal to zero" |> exn |> Error
              | _, 0, _ ->
                "resCount equal to zero" |> exn |> Error
              | _, _, Error (_, e) -> e |> Error
              | _, _, Ok x ->
                // do a heap of validation, i.e. TransactionIdentifier should equal the req TI
                if x.TransactionIdentifier <> transactionIdentifier then
                  "sprintf response is not to this request" |> exn |> Error
                else
                  match x.Response with
                  | ModErrorRes e ->
                    sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                    |> exn |> Error
                  | WriteDOsRes _ ->
                    () |> Ok
                  | _ -> exn "unexpected return function code" |> Error
          }
        )

      let readDos (initialReq : ReqOffQuant) = 
        lock lock_obj ( fun () ->
          async {

            transactionCounter <- transactionCounter + 1us
            let transactionIdentifier = transactionCounter
            let req = {
              TransactionIdentifier = transactionIdentifier
              ProtocolIdentifier = 0us
              UnitIdentifier = conf.SlaveId |> byte
              Request = initialReq |> RtuRequest.ReadDOReq
            }

            let req = req.Serialize () |> List.toArray |> ArraySegment
            let! reqCount = client.SendAsync(req, SocketFlags.None) |> Async.AwaitTask

            let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
            let! resCount = client.ReceiveAsync(resBuff, SocketFlags.None) |> Async.AwaitTask
            // todo: resCount = 0 should yield an exception

            let resBuff = resBuff.Slice(0, resCount).ToArray() |> Array.toList

            let res = MbapRes.TryParse(resBuff)
            return
              match reqCount, resCount, res with
              | 0, _, _ ->
                "reqCount equal to zero" |> exn |> Error
              | _, 0, _ ->
                "resCount equal to zero" |> exn |> Error
              | _, _, Error (_, e) -> e |> Error
              | _, _, Ok x ->
                // do a heap of validation, i.e. TransactionIdentifier should equal the req TI
                if x.TransactionIdentifier <> transactionIdentifier then
                  "sprintf response is not to this request" |> exn |> Error
                else
                  match x.Response with
                  | ModErrorRes e ->
                    sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                    |> exn |> Error
                  | ReadDORes x ->
                    x.Status |> List.take (initialReq.Quantity |> int) |> Ok
                  | _ -> exn "unexpected return function code" |> Error
          }
        )

      let writeRegs (offset, u16list) =
        lock lock_obj ( fun () ->
          async{
            let req : WriteRegsRequest =
              {
                Address = offset
                Values = u16list
              }
          
            transactionCounter <- transactionCounter + 1us
            let transactionIdentifier = transactionCounter
            let req = {
              TransactionIdentifier = transactionIdentifier
              ProtocolIdentifier = 0us
              UnitIdentifier = conf.SlaveId |> byte
              Request = req |> RtuRequest.WriteRegsReq
            }

            let req = req.Serialize () |> List.toArray |> ArraySegment
            let! reqCount = client.SendAsync(req, SocketFlags.None) |> Async.AwaitTask

            let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
            let! resCount = client.ReceiveAsync(resBuff, SocketFlags.None) |> Async.AwaitTask
            // todo: resCount = 0 should yield an exception

            let resBuff = resBuff.Slice(0, resCount).ToArray() |> Array.toList

            let res = MbapRes.TryParse(resBuff)
            return
              match reqCount, resCount, res with
              | 0, _, _ ->
                "reqCount equal to zero" |> exn |> Error
              | _, 0, _ ->
                "resCount equal to zero" |> exn |> Error
              | _, _, Error (_, e) -> e |> Error
              | _, _, Ok x ->
                // do a heap of validation, i.e. TransactionIdentifier should equal the req TI
                if x.TransactionIdentifier <> transactionIdentifier then
                  "sprintf response is not to this request" |> exn |> Error
                else
                  match x.Response with
                  | ModErrorRes e ->
                    sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                    |> exn |> Error
                  | WriteDOsRes _ ->
                    () |> Ok
                  | _ -> exn "unexpected return function code" |> Error
          }
        )

      return {
        ReadDOs = readDos
        ReadDIs = readDis
        ReadHRegs = readHRegs
        ReadIRegs = readIRegs
        WriteDOs = writeDos
        WriteRegs = writeRegs
      }
    }

        
    