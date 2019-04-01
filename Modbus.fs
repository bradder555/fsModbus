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

let modError = 
  {
    FunctionCode = Invalid 
    ExceptionCode = IllegalFunction 
  } |> ModErrorReq

let validatePdu (pdu : byte list) : Request = 
  let (Fc :: OffsetH :: OffsetL :: Rem) = pdu
  let functionCode = Fc |> ModbusTypes.FunctionCode.TryFromByte  
  match functionCode with
  | Ok functionCode -> 
    match functionCode with
    | FunctionCode.Invalid -> modError
    | FunctionCode.ReadDO -> 
      ReadDoRequest.TryParse pdu 
      |> function 
         | Ok x -> x |> ReadDOReq 
         | Error (p,e) -> modError

    | FunctionCode.ReadDI ->
      ReadDiRequest.TryParse pdu
      |> function 
         | Ok x -> x |> ReadDIReq 
         | Error (p,e) -> modError

    | FunctionCode.ReadHReg ->
      ReadHRegRequest.TryParse pdu 
      |> function 
         | Ok x -> x |> ReadHRegReq 
         | Error (p,e) -> modError

    | FunctionCode.ReadIReg -> 
      ReadIRegRequest.TryParse pdu
      |> function 
         | Ok x -> x |> ReadIRegReq 
         | Error (p,e) -> modError 

    | FunctionCode.WriteDO ->
      WriteDoRequest.TryParse pdu 
      |> function 
         | Ok x -> x |> WriteDOReq 
         | Error (p,e) -> modError

    | FunctionCode.WriteReg -> 
      WriteRegRequest.TryParse pdu
      |> function 
         | Ok x -> x |> WriteRegReq 
         | Error (p,e) -> modError

    | FunctionCode.WriteDOs ->
      WriteDosRequest.TryParse pdu
      |> function
         | Ok x -> x |> WriteDOsReq 
         | Error (p,e) -> modError

    | FunctionCode.WriteRegs -> 
      WriteRegsRequest.TryParse pdu
      |> function 
         | Ok x -> x |> WriteRegsReq 
         | Error (p,e) -> modError

  | Error _ -> 
    {
      FunctionCode = Invalid
      ExceptionCode = IllegalFunction 
    } |> ModErrorReq


let mapReqToRes (funcs : ModFuncs) (req : Request) : Response = 
  match req with
  | ReadDOReq x -> funcs.ReadDOFunc x |> ReadDORes
  | ReadDIReq x -> funcs.ReadDIFunc x |> ReadDIRes
  | ReadHRegReq x -> funcs.ReadHRegFunc x |> ReadHRegRes
  | ReadIRegReq x -> funcs.ReadIRegFunc x |> ReadIRegRes
  | WriteDOReq x -> funcs.WriteDOFunc x |> WriteDORes
  | WriteRegReq x -> funcs.WriteRegFunc x |> WriteRegRes
  | WriteRegsReq x -> funcs.WriteRegsFunc x |> WriteRegsRes
  | WriteDOsReq x -> funcs.WriteDOsFunc x |> WriteDOsRes
  | ModErrorReq x -> funcs.ModErrorFunc x |> ModErrorRes

open Hopac 
open System.Net
open System.Net.Sockets
let rec handleReceive (slaveId : byte) (handle : Socket) (funcs : ModFuncs) : Job<unit> = 
  job {
    let buff : Memory<byte> = Memory(Array.zeroCreate(300))
    let! len = handle.ReceiveAsync(buff, SocketFlags.None, CancellationToken(false)).AsTask()
    match handle.Connected && len <> 0 with 
      | true -> 
        let frame = buff.Slice(0, len).ToArray()
        let (pdu, mbap) = validateMbap slaveId frame 
        let request = validatePdu pdu
        let response = request |> mapReqToRes funcs
        let resPdu = response.Serialize()
        let rawRes = Mbap.wrapPdu mbap resPdu |> List.toArray 
        let outBuff = rawRes |> ReadOnlyMemory
        let! _ = handle.SendAsync(outBuff, SocketFlags.None, CancellationToken()).AsTask()
        do! handleReceive slaveId handle funcs
      | false -> 
        handle.Disconnect(true)
        handle.Dispose()
  } |> Job.startIgnore

let server (conf : ModbusServerConf) (actionFuncs : ModFuncs) : Job<unit> = 
  let listener = new Socket(SocketType.Stream, ProtocolType.Tcp)
  listener.Bind(IPEndPoint(conf.IPAddress, int conf.Port))
  listener.Listen(100)
  job {
    let! handler = listener.AcceptAsync()
    do! handleReceive conf.SlaveId handler actionFuncs
  } |> Job.foreverIgnore 