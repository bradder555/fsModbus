module Modbus
// for my code, the head should always be the least significant

open ModbusTypes
open System

open Hopac
open Hopac.Infixes

open System.Net
open System.Net.Sockets

let rec handleReceive (handle : Socket) (func : MbapFunc) : Job<unit> =
  job {
    let inBuff : ArraySegment<byte> = Array.zeroCreate(300) |> ArraySegment
    let mutable keepGoing = true
    while keepGoing do
      let! len = (fun () -> handle.ReceiveAsync(inBuff, SocketFlags.None )) |> Job.fromTask

      try
        match handle.Connected && len > 0 with
          | true ->
            let frame = inBuff.Slice(0, len).ToArray() |> Array.toList
            let reqMbap = MbapReq.TryParse frame
            match reqMbap with
            | Ok r ->
              let res = func r
              match res with
              | Error _ -> exn "error responding to request" |> raise
              | Ok res ->
                let buff =
                  res
                  |> (fun x -> x.Serialize())
                  |> List.toArray
                  |> ArraySegment

                do! (fun () -> handle.SendAsync(buff, SocketFlags.None)) |> Job.fromTask >>- ignore
            | Error _ ->
              exn "unable to parse request" |> raise
          | false ->
            exn "not connected to client" |> raise
      with
      | _ ->
        try
          handle.Disconnect(true)
        with | _ -> ()

        try
          handle.Dispose()
        with | _ -> ()

        keepGoing <- false

  } |> Job.startIgnore

let server (conf : ModbusServerConf) (actionFunc : MbapFunc) : Job<unit> =
  let listener = new Socket(SocketType.Stream, ProtocolType.Tcp)
  listener.Bind(IPEndPoint(conf.IPAddress, int conf.Port))
  listener.Listen(100)
  job {
    let! handler = listener.AcceptAsync()
    do! handleReceive handler actionFunc
  } |> Job.foreverIgnore