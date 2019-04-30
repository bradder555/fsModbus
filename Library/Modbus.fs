module Modbus
// for my code, the head should always be the least significant

open ModbusTypes

open Hopac
open Hopac.Infixes

open System
open System.Net
open System.Net.Sockets

let private disposeSocket (handle : Socket) =
  try
    handle.Close(0)
  with | _ -> ()

  try
    handle.Disconnect(true)
  with | _ -> ()

  try
    handle.Dispose()
  with | _ -> ()

module Server =
  let build (conf : ModbusServerConf) (actionFunc : MbapFunc) =

    // handle receive is only ever called from the server function
    // should not be in the external API, instead of using private, better
    // to scope it to this function exclusively
    let handleRec (handle : Socket) (inBuff : ArraySegment<byte>) (bufferLen : int) =
      job {
        try
          match handle.Connected && bufferLen > 0 with
            | true ->
              let frame = inBuff.Slice(0, bufferLen).ToArray() |> Array.toList
              let reqMbap = MbapReq.TryParse frame
              match reqMbap with
              | Ok r ->
                let res = actionFunc r
                match res with
                | Error _ -> exn "error responding to request" |> raise
                | Ok res ->
                  let buff =
                    res.Serialize()
                    |> List.toArray
                    |> ArraySegment

                  let! _ = handle.SendAsync(buff, SocketFlags.None)
                  return ()
              | Error _ ->
                exn "unable to parse request" |> raise
            | false ->
              exn "not connected to client" |> raise
        with
        | _ ->
          return disposeSocket handle
      }

    let rec handleReceive (handle : Socket) =
      job {
        let inBuff : ArraySegment<byte> = Array.zeroCreate(300) |> ArraySegment
        match handle.Connected with
        | true ->
          let! recLen = handle.ReceiveAsync(inBuff, SocketFlags.None)
          do! handleRec handle inBuff recLen
          do! handleReceive handle
        | false ->
          ()
      }

    let listener = new Socket(SocketType.Stream, ProtocolType.Tcp)
    listener.Bind(IPEndPoint(conf.IPAddress, int conf.Port))
    listener.Listen(100)

    let s =
      job {
        let! handler = listener.AcceptAsync()
        do! handleReceive handler |> Job.queue
      } |> Job.forever |> Promise.queue |> Alt.prepare

    Alt.withNackJob <| fun nack ->
      Job.start(
        nack
        >>- fun () ->
          disposeSocket listener
      ) >>-. s


(*
module Client =
  let build (shutdown : Alt<unit>) (conf : ModbusClientConf)  =
    let clientChannels =
      {
        ReadDOs = Ch()
        ReadDIs = Ch()
        ReadHRegs = Ch()
        ReadIRegs = Ch()
        WriteDOs = Ch()
        WriteRegs = Ch()
      }

    let connect =
      job {
        let client = new Socket(SocketType.Stream, ProtocolType.Tcp)
        let port = conf.Port |> int
        use connection =
          match conf.Server with
          | IPAddress' x ->
            client.ConnectAsync(x, port)
          | Hostname x ->
            client.ConnectAsync(x, port)
        do! (fun () -> connection ) |> Job.fromUnitTask
        return client
      }

    let rec loop (client : Socket) (transactionCounter : uint16 ) =
      job {
        let transactionCounter = transactionCounter + 1us
        let nextLoop = loop client transactionCounter

        do!
          Alt.choose [
            shutdown ^=> fun () -> disposeSocket client |> Job.result

            Ch.take clientChannels.ReadDIs
              ^=> fun (req, i) ->
                [
                  Job.tryIn
                    (
                      job{
                        let length =
                          req.PartialSerialize()
                          |> List.length
                          |> (+) 2
                          |> Convert.ToUInt16

                        let req = {
                          TransactionIdentifier = transactionCounter
                          ProtocolIdentifier = 0us
                          UnitIdentifier = conf.SlaveId |> byte
                          Request = req |> RtuRequest.ReadDIReq
                          Length = length
                        }
                        let req = req.Serialize () |> List.toArray |> ArraySegment
                        let! reqCount = (fun () -> client.SendAsync(req, SocketFlags.None)) |> Job.fromTask
                        let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
                        let! resCount = (fun () -> client.ReceiveAsync(resBuff, SocketFlags.None)) |> Job.fromTask
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
                            match x.Response with
                            | ModErrorRes e ->
                              sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                              |> exn |> Error
                            | ReadDIRes x ->
                              x.Status |> Ok
                            | _ -> exn "unexpected return function code" |> Error
                      }
                    )
                    (IVar.fill i)
                    (IVar.fillFailure i)
                  nextLoop
                ] |> Job.seqIgnore

            Ch.take clientChannels.WriteDOs
              ^=> fun ((offset, blist), i) ->
                [
                  Job.tryIn
                    (
                      job{
                        let req : WriteDosRequest =
                          {
                            Address = offset
                            Quantity = blist |> List.length |> Convert.ToUInt16
                            Values = blist
                          }

                        let length =
                          req.Serialize()
                          |> List.length
                          |> (+) 1
                          |> Convert.ToUInt16

                        let req = {
                          TransactionIdentifier = transactionCounter
                          ProtocolIdentifier = 0us
                          UnitIdentifier = conf.SlaveId |> byte
                          Request = req |> RtuRequest.WriteDOsReq
                          Length = length
                        }

                        let req = req.Serialize () |> List.toArray |> ArraySegment
                        let! reqCount = (fun () -> client.SendAsync(req, SocketFlags.None)) |> Job.fromTask

                        let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
                        let! resCount = (fun () -> client.ReceiveAsync(resBuff, SocketFlags.None)) |> Job.fromTask
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
                            match x.Response with
                            | ModErrorRes e ->
                              sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                              |> exn |> Error
                            | WriteDOsRes _ ->
                              () |> Ok
                            | _ -> exn "unexpected return function code" |> Error
                      }
                    )
                    (IVar.fill i)
                    (IVar.fillFailure i)
                  nextLoop
                ] |> Job.seqIgnore

            Ch.take clientChannels.ReadDOs
              ^=> fun (req, i) ->
                [
                  Job.tryIn
                    (
                      job{
                        let length =
                          req.PartialSerialize()
                          |> List.length
                          |> (+) 2
                          |> Convert.ToUInt16

                        let req = {
                          TransactionIdentifier = transactionCounter
                          ProtocolIdentifier = 0us
                          UnitIdentifier = conf.SlaveId |> byte
                          Request = req |> RtuRequest.ReadDOReq
                          Length = length
                        }

                        let req = req.Serialize () |> List.toArray |> ArraySegment
                        let! reqCount = (fun () -> client.SendAsync(req, SocketFlags.None)) |> Job.fromTask

                        let resBuff : ArraySegment<byte> = Array.zeroCreate(500) |> ArraySegment
                        let! resCount = (fun () -> client.ReceiveAsync(resBuff, SocketFlags.None)) |> Job.fromTask
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
                            match x.Response with
                            | ModErrorRes e ->
                              sprintf "ModError with FC = %A and EC = %A" e.ExceptionCode e.FunctionCode
                              |> exn |> Error
                            | ReadDORes x ->
                              x.Status |> Ok
                            | _ -> exn "unexpected return function code" |> Error
                      }
                    )
                    (IVar.fill i)
                    (IVar.fillFailure i)
                  nextLoop
                ] |> Job.seqIgnore
          ]
      }

    connect
    >>= (fun c -> loop c 0us)
    >>-. clientChannels
*)