module Modbus
// for my code, the head should always be the least significant

open ModbusTypes
open System
open System.Threading
open Util

let validateMbap (slaveId : byte) (frame : byte []) : PDU * Mbap =
  // the smallest frame is 12 bytes long
  if frame.Length < 12 then
    FormatException("Frame length less than the minimum of 12") |> raise
  let frame = frame |> Array.toList
  let (TidH :: TidL :: PiH :: PiL :: LH :: LL :: Ui :: Pdu) = frame
  let length = [LL; LH; 0uy; 0uy] |> tI32
  let lengthU = [LL; LH] |> tU16
  let mbap : Mbap =
    {
      Length = lengthU
      ProtocolIdentifier = [PiL; PiH] |> tU16
      TransactionIdentifier = [TidL; TidH] |> tU16
      UnitIdentifier = Ui
    }

  // verify the length
  if (frame.Length - 6) <> length then
    FormatException("Frame is reporting a different length than received") |> raise

  if slaveId <> Ui then
    FormatException(sprintf "This slave is %i, requested slave is %i" slaveId Ui) |> raise

  if PiL <> 0uy || PiH <> 0uy then
    FormatException("Protocol identifier is expected to be zero") |> raise

  Pdu, mbap

open Hopac
open System.Net
open System.Net.Sockets
let rec handleReceive (slaveId : byte) (handle : Socket) (func : ModFunc) : Job<unit> =
  job {
    let buff : Memory<byte> = Memory(Array.zeroCreate(300))
    let! len = handle.ReceiveAsync(buff, SocketFlags.None, CancellationToken(false)).AsTask()
    match handle.Connected && len <> 0 with
      | true ->
        let frame = buff.Slice(0, len).ToArray()
        let (pdu, mbap) = validateMbap slaveId frame
        let request = Request.TryParse pdu
        match request with
        | Ok r ->
          let response = r |> func
          let resPdu = response.Serialize()
          let rawRes = Mbap.wrapPdu mbap resPdu |> List.toArray
          let outBuff = rawRes |> ReadOnlyMemory
          let! _ = handle.SendAsync(outBuff, SocketFlags.None, CancellationToken()).AsTask()
          do! handleReceive slaveId handle func
        | Error _ ->
          handle.Disconnect(true)
          handle.Dispose()
      | false ->
        handle.Disconnect(true)
        handle.Dispose()
  } |> Job.startIgnore

let server (conf : ModbusServerConf) (actionFunc : ModFunc) : Job<unit> =
  let listener = new Socket(SocketType.Stream, ProtocolType.Tcp)
  listener.Bind(IPEndPoint(conf.IPAddress, int conf.Port))
  listener.Listen(100)
  job {
    let! handler = listener.AcceptAsync()
    do! handleReceive conf.SlaveId handler actionFunc
  } |> Job.foreverIgnore