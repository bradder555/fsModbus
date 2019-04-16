module ModbusTypes
#nowarn "25"

// for my code, the head should always be the least significant

open Util
open System

type TransactionIdentifier = System.UInt16
type ProtocolIdentifier = System.UInt16
type Length = System.UInt16
type UnitIdentifier = System.Byte

type PDU = byte list

// mbap header = TransactionIdentifier, ProtocolIdentifier (always zero), Length, UnitIdentifier (slave id, typically 1)


type FunctionCode =
  | ReadDO
  | ReadDI
  | ReadHReg
  | ReadIReg
  | WriteDO
  | WriteReg
  | WriteDOs
  | WriteRegs
  member x.ToByte () : byte =
    match x with
    | ReadDO -> 1uy
    | ReadDI -> 2uy
    | ReadHReg -> 3uy
    | ReadIReg -> 4uy
    | WriteDO -> 5uy
    | WriteReg -> 6uy
    | WriteDOs -> 15uy
    | WriteRegs -> 16uy

  static member TryFromByte (x : byte) : Result<FunctionCode,byte> =
    match x with
    | 1uy -> Ok ReadDO
    | 2uy -> Ok ReadDI
    | 3uy -> Ok ReadHReg
    | 4uy -> Ok ReadIReg
    | 5uy -> Ok WriteDO
    | 6uy -> Ok WriteReg
    | 15uy -> Ok WriteDOs
    | 16uy -> Ok WriteRegs
    | y -> Error y

type ExceptionCode =
  | IllegalFunction     // function code not recognised
  | IllegalDataAddress  // address not allowed
  | IllegalDataValue    // value not allowed
  | SlaveDeviceFailure  // unrecoverable error
  | SlaveDeviceBusy     // slave is busy and unable to respond
  | PathUnavailable     // gateway misconfigured or overloaded
  | DeviceFailedToRespond  // no response from gateway
  member x.ToByte () : byte =
    match x with
      | IllegalFunction    -> 1uy
      | IllegalDataAddress -> 2uy
      | IllegalDataValue   -> 3uy
      | SlaveDeviceFailure -> 4uy
      | SlaveDeviceBusy    -> 6uy
      | PathUnavailable    -> 10uy
      | DeviceFailedToRespond -> 11uy

  static member TryFromByte (x : byte) : Result<ExceptionCode,byte> =
    match x with
    | 1uy -> Ok IllegalFunction
    | 2uy -> Ok IllegalDataAddress
    | 3uy -> Ok IllegalDataValue
    | 4uy -> Ok SlaveDeviceFailure
    | 6uy -> Ok SlaveDeviceBusy
    | 10uy -> Ok PathUnavailable
    | 16uy -> Ok DeviceFailedToRespond
    | _ -> Error x
type ModError =
  {
    FunctionCode : FunctionCode
    ExceptionCode : ExceptionCode
  }
  member x.Serialize () =
    let exCode = x.FunctionCode.ToByte() + 128uy
    let erCode = x.ExceptionCode.ToByte()
    [exCode; erCode]

  static member TryParse (pdu : PDU) : Result<ModError, PDU * exn> =
    try
      let (fc :: ec :: []) = pdu
      let fc =
        match fc with
        | x when x &&& 0x80uy = 0x80uy -> x - 0x80uy |> Ok
        | x -> "Error flag not set" |> FormatException |> Error

      let fc =
        match fc with
        | Ok x ->
          FunctionCode.TryFromByte x
          |> function
             | Error _ -> "Function code invalid" |> FormatException |> Error
             | Ok x -> x |> Ok
        | Error x -> x |> Error

      let ec = ExceptionCode.TryFromByte ec
      match fc,ec with
      | Error e1, Error _ ->
        sprintf "%s, Exception code invalid" e1.Message |> FormatException |> raise
      | Error e, _ ->
        e |> raise
      | _, Error _ ->
        "Exception code invalid" |> FormatException |> raise
      | Ok fc, Ok ec ->
        {
          FunctionCode = fc
          ExceptionCode = ec
        } |> Ok
    with | e ->
      (pdu, e) |> Error

type ReqOffQuant = 
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member TryParse (pdu : PDU) : Result<ReqOffQuant,PDU * exn> =
    try
      let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
      {
        Offset = [addrL; addrH] |> tU16
        Quantity = [countL; countH] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.PartialSerialize () =
    let [addrL; addrH] = x.Offset |> Util.U16ToBytes
    let [countL; countH] = x.Quantity |> Util.U16ToBytes
    let fc = FunctionCode.ReadDO.ToByte()
    [addrH; addrL; countH; countL]

type WriteDoRequest =
  {
    Offset : System.UInt16
    Value : bool
  }
  static member TryParse (pdu : byte list) : Result<WriteDoRequest, PDU * exn > =
    try
      let (fc :: addrH :: addrL :: valueH :: valueL :: []) = pdu // throw exception if not an exact match
      {
        Offset = [addrL; addrH] |> tU16
        Value = [valueL; valueH] |> tU16 > 0us
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.Serialize () =
    let [addrL; addrH] = x.Offset |> Util.U16ToBytes
    let [valL; valH] = x.Value |> Util.BoolToUint16 |> Util.U16ToBytes
    let fc = FunctionCode.WriteDO.ToByte()
    [fc; addrH; addrL; valH; valL]

type WriteRegRequest =
  {
    Offset : System.UInt16
    Value : System.UInt16
  }
  static member TryParse (pdu : byte list) : Result<WriteRegRequest, PDU * exn> =
    try
      let (fc :: addrH :: addrL :: valueH :: valueL :: []) = pdu // throw exception if not an exact match
      {
        Offset = [addrL; addrH] |> tU16
        Value = [valueL; valueH] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.Serialize () =
    let [addrL; addrH] = x.Offset |> Util.U16ToBytes
    let [valL; valH] = x.Value |> Util.U16ToBytes
    let fc = FunctionCode.WriteReg.ToByte()
    [fc; addrH; addrL; valH; valL]

type WriteDosRequest =
  {
    Offset : System.UInt16
    Quantity: System.UInt16
    // cannot be inferred, this information is needed
    // by the response
    // ByteCount: byte // useful for parsing validation, though
    Values: bool list
  }
  static member TryParse (pdu : byte list) : Result<WriteDosRequest, PDU * exn> =
    try
      let (fc :: addrH :: addrL :: countH :: countL :: byteCount :: values) = pdu // throw exception if not an exact match
      {
        Offset = [addrL; addrH] |> tU16
        Quantity = [countL; countH] |> tU16
        Values = values |> Util.bytesToBool
      } |> Ok
    with | e -> (pdu, e) |> Error
    member x.Serialize () =
      let [addrL; addrH] = x.Offset |> Util.U16ToBytes
      let [countL; countH] = x.Quantity |> Util.U16ToBytes
      let data = x.Values |> Util.BoolsToBytes
      let byteCount = data |> List.length |> byte
      let fc = FunctionCode.WriteDOs.ToByte()
      [fc; addrH; addrL; countH; countL; byteCount] @ data

type WriteRegsRequest =
  {
    Offset : System.UInt16
    Quantity : System.UInt16
    //ByteCount: byte
    Values : System.UInt16 list
  }
  static member TryParse (pdu : byte list) : Result<WriteRegsRequest,PDU * exn> =
    try
      let (fc :: addrH :: addrL :: countH :: countL :: count :: values) = pdu // throw exception if not an exact match
      if values.Length % 2 <> 0 then
        "Even number of data bytes expected" |> FormatException |> raise

      {
        Offset = [addrL; addrH] |> tU16
        Quantity = [countL; countH] |> tU16
        Values = values |> swapU16s |> Util.bytesToUint16
      } |> Ok
    with | e -> (pdu, e) |> Error
    member x.Serialize () =
      let [addrL; addrH] = x.Offset |> Util.U16ToBytes
      let [countL; countH] = x.Quantity |> Util.U16ToBytes
      let data = x.Values |> Util.U16sToBytes |> Util.swapU16s
      let byteCount = data |> List.length |> byte
      let fc = FunctionCode.WriteRegs.ToByte()
      [fc; addrH; addrL; countH; countL; byteCount] @ data

type Request =
  | ReadDOReq of ReqOffQuant
  | ReadDIReq of ReqOffQuant
  | ReadHRegReq of ReqOffQuant
  | ReadIRegReq of ReqOffQuant
  | WriteDOReq of WriteDoRequest
  | WriteRegReq of WriteRegRequest
  | WriteDOsReq of WriteDosRequest
  | WriteRegsReq of WriteRegsRequest
  | ModErrorReq of ModError
  static member TryParse (pdu : byte list) : Result<Request, PDU> = 
    let (Fc :: OffsetH :: OffsetL :: Rem) = pdu
    let functionCode = Fc |> FunctionCode.TryFromByte
    let modError fc =
      {
        FunctionCode = fc
        ExceptionCode = IllegalFunction
      } |> ModErrorReq
    match functionCode with
    | Ok functionCode ->
      match functionCode with
      | FunctionCode.ReadDO ->
        ReqOffQuant.TryParse pdu
        |> function
           | Ok x -> x |> ReadDOReq |> Ok
           | Error (p,e) -> modError FunctionCode.ReadDO |> Ok

      | FunctionCode.ReadDI ->
        ReqOffQuant.TryParse pdu
        |> function
           | Ok x -> x |> ReadDIReq |> Ok
           | Error (p,e) -> modError FunctionCode.ReadDI |> Ok

      | FunctionCode.ReadHReg ->
        ReqOffQuant.TryParse pdu
        |> function
           | Ok x -> x |> ReadHRegReq |> Ok
           | Error (p,e) -> modError FunctionCode.ReadHReg |> Ok

      | FunctionCode.ReadIReg ->
        ReqOffQuant.TryParse pdu
        |> function
           | Ok x -> x |> ReadIRegReq |> Ok
           | Error (p,e) -> modError FunctionCode.ReadIReg |> Ok

      | FunctionCode.WriteDO ->
        WriteDoRequest.TryParse pdu
        |> function
           | Ok x -> x |> WriteDOReq |> Ok
           | Error (p,e) -> modError FunctionCode.WriteDO |> Ok

      | FunctionCode.WriteReg ->
        WriteRegRequest.TryParse pdu
        |> function
           | Ok x -> x |> WriteRegReq |> Ok
           | Error (p,e) -> modError FunctionCode.WriteReg |> Ok

      | FunctionCode.WriteDOs ->
        WriteDosRequest.TryParse pdu
        |> function
           | Ok x -> x |> WriteDOsReq |> Ok
           | Error (p,e) -> modError FunctionCode.WriteDOs |> Ok

      | FunctionCode.WriteRegs ->
        WriteRegsRequest.TryParse pdu
        |> function
           | Ok x -> x |> WriteRegsReq |> Ok
           | Error (p,e) -> modError FunctionCode.WriteRegs |> Ok

    | Error _ ->
      pdu |> Error
  member x.Serialize () : byte list = 
    match x with 
    | ReadDOReq x -> FunctionCode.ReadDO.ToByte() :: x.PartialSerialize()
    | ReadDIReq x -> FunctionCode.ReadDI.ToByte() :: x.PartialSerialize()
    | ReadHRegReq x -> FunctionCode.ReadHReg.ToByte() :: x.PartialSerialize()
    | ReadIRegReq x -> FunctionCode.ReadIReg.ToByte() ::  x.PartialSerialize()
    | WriteDOReq x -> x.Serialize()
    | WriteRegReq x -> x.Serialize()
    | WriteDOsReq x -> x.Serialize()
    | WriteRegsReq x -> x.Serialize()
    | ModErrorReq x -> x.Serialize()

type ResBools =
  {
    Status: bool list
  }
  member x.PartialSerialize ()  : byte list =
    let status = x.Status |> Util.BoolsToBytes
    let c = status |> List.length |> byte
    c :: status

  static member TryParse (pdu : PDU) : Result<ResBools, PDU * exn> =
    try
      let (fc :: count :: data) = pdu // throw exception if not an exact match
      {
        Status = data |> bytesToBool
      } |> Ok
    with | e -> (pdu, e) |> Error

type ResRegs =
  {
    Values : UInt16 list
  }
  member x.PartialSerialize ()  : byte list =
    let data = x.Values |> Util.U16sToBytes |> swapU16s
    let count = data |> List.length |> byte

    count :: data

  static member TryParse (pdu : PDU) : Result<ResRegs, PDU * exn> =
    try
      let (fc :: count :: data) = pdu // throw exception if not an exact match
      {
        Values = data |> bytesToUint16
      } |> Ok
    with | e -> (pdu, e) |> Error


type WriteDoResponse =
  {
    Offset: UInt16
    Value: bool
  }
  member x.Serialize ()  : byte list =
    let oBytes = x.Offset |> U16ToBytes |> swapU16s
    let oVal = x.Value |> BoolToUint16 |> U16ToBytes |> swapU16s
    let fc = FunctionCode.WriteDO.ToByte()
    fc :: oBytes @ oVal

  static member TryParse (pdu : PDU) : Result<WriteDoResponse, PDU * exn>  =
    try
      let (fc :: offsetH :: offsetL :: valH :: valL :: []) = pdu // throw exception if not an exact match
      {
        Offset = [offsetH; offsetL] |> tU16
        Value = valH = 255uy
      } |> Ok

    with | e -> (pdu, e) |> Error


type WriteRegResponse =
  {
    Offset: UInt16
    Value: UInt16
  }
  member x.Serialize ()  : byte list =
    let oBytes = x.Offset |> Util.U16ToBytes |> swapU16s
    let oVal = x.Value |> Util.U16ToBytes |> swapU16s
    let fc = FunctionCode.WriteReg.ToByte()
    fc :: oBytes @ oVal

type ResOffQuant =
  {
    Offset: UInt16
    Quantity: UInt16
  }
  member x.PartialSerialize ()  : byte list =
    let oOffset = x.Offset |> Util.U16ToBytes |> swapU16s
    let oCount = x.Quantity |> Util.U16ToBytes |> swapU16s
    oOffset @ oCount

type Response =
  | ReadDORes of ResBools
  | ReadDIRes of ResBools
  | ReadHRegRes of ResRegs
  | ReadIRegRes of ResRegs
  | WriteDORes of WriteDoResponse
  | WriteRegRes of WriteRegResponse
  | WriteDOsRes of ResOffQuant
  | WriteRegsRes of ResOffQuant
  | ModErrorRes of ModError
  member x.Serialize ()  : byte list =
    match x with
    | ReadDORes x -> FunctionCode.ReadDO.ToByte() :: x.PartialSerialize ()
    | ReadDIRes x -> FunctionCode.ReadDI.ToByte() :: x.PartialSerialize ()
    | ReadHRegRes x -> FunctionCode.ReadHReg.ToByte() :: x.PartialSerialize ()
    | ReadIRegRes x -> FunctionCode.ReadIReg.ToByte() :: x.PartialSerialize ()
    | WriteDORes x -> x.Serialize ()
    | WriteRegRes x -> x.Serialize ()
    | WriteDOsRes x -> FunctionCode.WriteDOs.ToByte() :: x.PartialSerialize ()
    | WriteRegsRes x -> FunctionCode.WriteRegs.ToByte() :: x.PartialSerialize ()
    | ModErrorRes x -> x.Serialize ()

  // todo: need to add response parsing

type MbapPayload = 
  | Response of Response
  | Request of Request

type Mbap =
  {
    TransactionIdentifier : TransactionIdentifier
    ProtocolIdentifier : ProtocolIdentifier
    Length : Length
    UnitIdentifier : UnitIdentifier
    Payload : MbapPayload
  }
  member x.Serialize ()  : byte list =
    // The transaction and protocol identifiers are reversed
    let ti = x.TransactionIdentifier |> U16ToBytes |> swapU16s
    let pi = x.ProtocolIdentifier |> U16ToBytes |> swapU16s
    let len = x.Length |> U16ToBytes |> swapU16s
    let uid = x.UnitIdentifier

    ti @ pi @ len @ [uid]

  static member wrapPdu (reqMbap : Mbap) (pdu : PDU) : byte list =
    let nlen = pdu |> List.length |> uint16 |> (+)1us
    let nMbap =
      {
        TransactionIdentifier = reqMbap.TransactionIdentifier
        ProtocolIdentifier = reqMbap.ProtocolIdentifier
        Length = nlen
        UnitIdentifier = reqMbap.UnitIdentifier
      }
    nMbap.Serialize () @ pdu @ []

// delegates that are called for each type of function
type ModFunc = Request -> Response
module ModFunc = 
  let private expected q =
    q / 8us + 1us
    |> fun x -> [1us..x]
    |> List.map (fun _ -> 0uy)
    |> fun x -> 1uy :: (x |> List.tail)
    |> Util.bytesToBool

  let private expected16 (q : UInt16) =
    [1us..q]
    |> List.map(fun _ -> 0us)
    |> fun x -> 1us :: (x |> List.tail)

  module Default = 
    module Success = 
      let readDOFunc (req : ReqOffQuant) : Response = 
        {
          Status = expected req.Quantity
        } |> ReadDORes
      let readDIFunc (req : ReqOffQuant) : Response = 
        {
          Status = expected req.Quantity
        } |> ReadDIRes
      let readHRegFunc (req : ReqOffQuant) : Response = 
        {
          Values = expected16 req.Quantity
        } |> ReadHRegRes
      let readIRegFunc (req : ReqOffQuant) : Response = 
        {
          Values = expected16 req.Quantity
        } |> ReadIRegRes
      let writeDOFunc (req : WriteDoRequest) : Response = 
        let r : WriteDoResponse = 
          {
            Offset = req.Offset
            Value = req.Value
          } 
        r |> WriteDORes
      let writeRegFunc (req : WriteRegRequest) : Response = 
        {
          Offset = req.Offset
          Value = req.Value
        } |> WriteRegRes
      
      let writeDOsFunc (req : WriteDosRequest) : Response = 
        {
          Offset = req.Offset
          Quantity = req.Quantity
        } |> WriteDOsRes

      let writeRegsFunc (req : WriteRegsRequest) : Response = 
        {
          Offset = req.Offset
          Quantity = req.Quantity
        } |> WriteRegsRes

  // the defaults do nothing, all they do is return is
  // the expected corresponding successful response
  let defaultSuccesses =
    let r (req : Request) : Response = 
      match req with
      | ReadDOReq x -> Default.Success.readDOFunc x 
      | ReadDIReq x -> Default.Success.readDIFunc x 
      | ReadHRegReq x -> Default.Success.readHRegFunc x 
      | ReadIRegReq x -> Default.Success.readIRegFunc x 
      | WriteDOReq x -> Default.Success.writeDOFunc x 
      | WriteRegReq x -> Default.Success.writeRegFunc x 
      | WriteDOsReq x -> Default.Success.writeDOsFunc x 
      | WriteRegsReq x -> Default.Success.writeRegsFunc x  

    r

type ModbusServerConf =
  {
    Port : System.UInt32
    IPAddress : System.Net.IPAddress
    SlaveId: byte
  }
  static member TryParse (str : string) : Result<ModbusServerConf, string> =
    // string expected in the format tcp://127.0.0.1:502
    let str = str.ToLower()
    match str.IndexOf("tcp://") with
    | 0 ->
      let str1 = str.Split("tcp://") |> Array.last
      match str1.Split(":") with
      | [|addr; port|] ->
        let resIP = ref (System.Net.IPAddress(0L))
        let successIP = System.Net.IPAddress.TryParse(addr, resIP)
        let resPort = ref 0u
        let successPort = System.UInt32.TryParse(port, resPort)
        match successIP, successPort  with
        | true, true ->
          {
            Port = resPort.Value
            IPAddress = resIP.Value
            SlaveId = 1uy
          } |> Ok
        | _ -> Error str
      | _ -> Error str
    | _ -> Error str
