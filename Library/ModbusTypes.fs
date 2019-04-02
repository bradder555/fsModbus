module ModbusTypes
open Expecto.CSharp
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
  | Invalid
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
    | Invalid -> 0uy
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
    | 0uy -> Ok Invalid
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

type ModError =
  {
    FunctionCode : FunctionCode
    ExceptionCode : ExceptionCode
  }
  member x.Serialize () =
    let exCode = x.FunctionCode.ToByte() + 128uy
    let erCode = x.ExceptionCode.ToByte()
    [exCode; erCode]

type ReadDoRequest =
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member TryParse (pdu : PDU) : Result<ReadDoRequest,PDU * exn> =
    try
      let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
      {
        Offset = [addrL; addrH] |> tU16
        Quantity = [countL; countH] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.Serialize () =
    let [addrL; addrH] = x.Offset |> Util.U16ToBytes
    let [countL; countH] = x.Quantity |> Util.U16ToBytes
    let fc = FunctionCode.ReadDO.ToByte()
    [fc; addrH; addrL; countH; countL]

type ReadDiRequest =
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member TryParse (pdu : PDU) : Result<ReadDiRequest,PDU * exn> =
    try
      let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
      {
        Offset = [addrL; addrH] |> tU16
        Quantity = [countL; countH] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.Serialize () =
    let [addrL; addrH] = x.Offset |> Util.U16ToBytes
    let [countL; countH] = x.Quantity |> Util.U16ToBytes
    let fc = FunctionCode.ReadDI.ToByte()
    [fc; addrH; addrL; countH; countL]

type ReadHRegRequest =
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member TryParse (pdu : byte list) : Result<ReadHRegRequest,PDU * exn> =
    try
      let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
      {
        Offset = [addrL; addrH] |> tU16
        Quantity = [countL; countH] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error

  member x.Serialize () = // serialization can't fail
    let [addrL; addrH] = x.Offset |> Util.U16ToBytes
    let [countL; countH] = x.Quantity |> Util.U16ToBytes
    let fc = FunctionCode.ReadHReg.ToByte()
    [fc; addrH; addrL; countH; countL]

type ReadIRegRequest =
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member TryParse (pdu : byte list) : Result<ReadIRegRequest, PDU * exn > =
    try
      let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
      {
        Offset = [addrL; addrH] |> tU16
        Quantity = [countL; countH] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.Serialize () =
    let [addrL; addrH] = x.Offset |> Util.U16ToBytes
    let [countL; countH] = x.Quantity |> Util.U16ToBytes
    let fc = FunctionCode.ReadIReg.ToByte()
    [fc; addrH; addrL; countH; countL]

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
  | ReadDOReq of ReadDoRequest
  | ReadDIReq of ReadDiRequest
  | ReadHRegReq of ReadHRegRequest
  | ReadIRegReq of ReadIRegRequest
  | WriteDOReq of WriteDoRequest
  | WriteRegReq of WriteRegRequest
  | WriteDOsReq of WriteDosRequest
  | WriteRegsReq of WriteRegsRequest
  | ModErrorReq of ModError

type ReadDoResponse =
  {
    Status: bool list
  }
  member x.Serialize ()  : byte list =
    let status = x.Status |> Util.BoolsToBytes
    let c = status |> List.length |> byte
    let fc = FunctionCode.ReadDO.ToByte()
    [fc; c] @ status

  static member TryParse (pdu : PDU) : Result<ReadDoResponse, PDU * exn> =
    try
      let (fc :: count :: data) = pdu // throw exception if not an exact match
      {
        Status = data |> bytesToBool
      } |> Ok
    with | e -> (pdu, e) |> Error

type ReadDiResponse =
  {
    Status: bool list
  }
  member x.Serialize ()  : byte list =
    let status = x.Status |> Util.BoolsToBytes
    let c = status |> List.length |> byte
    let fc = FunctionCode.ReadDI.ToByte()
    [fc; c] @ status

  static member TryParse (pdu : PDU) : Result<ReadDiResponse, PDU * exn> =
    try
      let (fc :: count :: data) = pdu // throw exception if not an exact match
      {
        Status = data |> bytesToBool
      } |> Ok
    with | e -> (pdu, e) |> Error

type ReadHRegResponse =
  {
    Values : UInt16 list
  }
  member x.Serialize ()  : byte list =
    let data = x.Values |> Util.U16sToBytes |> swapU16s
    let count = data |> List.length |> byte

    [3uy; count] @ data

  static member TryParse (pdu : PDU) : Result<ReadHRegResponse, PDU * exn> =
    try
      let (fc :: count :: data) = pdu // throw exception if not an exact match
      {
        Values = data |> bytesToUint16
      } |> Ok
    with | e -> (pdu, e) |> Error

type ReadIRegResponse =
  {
    Values : UInt16 list
  }
  member x.Serialize ()  : byte list =
    let data = x.Values |> Util.U16sToBytes |> swapU16s
    let count = data |> List.length |> byte
    let fc = FunctionCode.ReadIReg.ToByte()
    [fc; count] @ data

  static member TryParse (pdu : PDU) : Result<ReadIRegResponse, PDU * exn> =
    try
      let (fc :: count :: data ) = pdu // throw exception if not an exact match
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

type WriteDosResponse =
  {
    Offset: UInt16
    Count: UInt16
  }
  member x.Serialize ()  : byte list =
    let oOffset = x.Offset |> Util.U16ToBytes |> swapU16s
    let oCount = x.Count |> Util.U16ToBytes |> swapU16s
    let fc = FunctionCode.WriteDOs.ToByte()
    fc :: oOffset @ oCount

type WriteRegsResponse =
  {
    Offset: UInt16
    Count: UInt16
  }
  member x.Serialize ()  : byte list =
    let oOffset = x.Offset |> U16ToBytes |> swapU16s
    let oCount = x.Count |> U16ToBytes |> swapU16s
    [16uy] @ oOffset @ oCount

  static member TryFromByte (x : byte) : Result<ExceptionCode,byte> =
    match x with
    | 1uy -> Ok IllegalFunction
    | 2uy -> Ok IllegalDataAddress
    | 3uy -> Ok IllegalDataValue
    | 4uy -> Ok SlaveDeviceFailure
    | 6uy -> Ok SlaveDeviceBusy
    | 10uy -> Ok PathUnavailable
    | 11uy -> Ok DeviceFailedToRespond
    | y -> Error y

type Response =
  | ReadDORes of ReadDoResponse
  | ReadDIRes of ReadDiResponse
  | ReadHRegRes of ReadHRegResponse
  | ReadIRegRes of ReadIRegResponse
  | WriteDORes of WriteDoResponse
  | WriteRegRes of WriteRegResponse
  | WriteDOsRes of WriteDosResponse
  | WriteRegsRes of WriteRegsResponse
  | ModErrorRes of ModError
  member x.Serialize ()  : byte list =
    match x with
    | ReadDORes x -> x.Serialize ()
    | ReadDIRes x -> x.Serialize ()
    | ReadHRegRes x -> x.Serialize ()
    | ReadIRegRes x -> x.Serialize ()
    | WriteDORes x -> x.Serialize ()
    | WriteRegRes x -> x.Serialize ()
    | WriteDOsRes x -> x.Serialize ()
    | WriteRegsRes x -> x.Serialize ()
    | ModErrorRes x -> x.Serialize ()

type Mbap =
  {
    TransactionIdentifier : TransactionIdentifier
    ProtocolIdentifier : ProtocolIdentifier
    Length : Length
    UnitIdentifier : UnitIdentifier
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
type ModFuncs =
  {
    ReadDOFunc    : ReadDoRequest -> ReadDoResponse
    ReadDIFunc    : ReadDiRequest -> ReadDiResponse
    ReadHRegFunc  : ReadHRegRequest -> ReadHRegResponse
    ReadIRegFunc  : ReadIRegRequest -> ReadIRegResponse
    WriteDOFunc   : WriteDoRequest -> WriteDoResponse
    WriteRegFunc  : WriteRegRequest -> WriteRegResponse
    WriteDOsFunc  : WriteDosRequest -> WriteDosResponse
    WriteRegsFunc : WriteRegsRequest -> WriteRegsResponse
    ModErrorFunc  : ModError -> ModError
  }
  // the defaults do nothing, all they do is return is
  // the expected corresponding successful response
  static member defaultSuccesses =
    let expected q =
      q / 8us + 1us
      |> fun x -> [1us..x]
      |> List.map (fun _ -> 0uy)
      |> fun x -> 1uy :: (x |> List.tail)
      |> Util.bytesToBool

    let expected16 (q : UInt16) =
      [1us..q]
      |> List.map(fun _ -> 0us)
      |> fun x -> 1us :: (x |> List.tail)

    let readDoFunc (x : ReadDoRequest) : ReadDoResponse =
      {
        Status = expected x.Quantity
      }

    let readDiFunc (x : ReadDiRequest) : ReadDiResponse =
      {
        Status = expected x.Quantity
      }

    let readHRegFunc (x : ReadHRegRequest) : ReadHRegResponse =
      {
        Values = expected16 x.Quantity
      }

    let readIRegFunc (x : ReadIRegRequest) : ReadIRegResponse =
      {
        Values = expected16 x.Quantity
      }

    let writeDoFunc (x : WriteDoRequest) : WriteDoResponse =
      {
        Offset = x.Offset
        Value = x.Value
      }

    let writeRegFunc (x : WriteRegRequest) : WriteRegResponse =
      {
        Offset = x.Offset
        Value = x.Value
      }

    let writeDOsFunc (x : WriteDosRequest) : WriteDosResponse =
      {
        Count = x.Quantity
        Offset = x.Offset
      }

    let writeRegsFunc (x : WriteRegsRequest) : WriteRegsResponse =
      {
        Count = x.Quantity
        Offset = x.Offset
      }
    {
      ReadDOFunc    = readDoFunc
      ReadDIFunc    = readDiFunc
      ReadHRegFunc  = readHRegFunc
      ReadIRegFunc  = readIRegFunc
      WriteDOFunc   = writeDoFunc
      WriteRegFunc  = writeRegFunc
      WriteDOsFunc  = writeDOsFunc
      WriteRegsFunc = writeRegsFunc
      ModErrorFunc  = fun x -> x
    }


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
