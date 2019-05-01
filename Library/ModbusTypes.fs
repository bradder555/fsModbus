module ModbusTypes
open System.Transactions
open Hopac
#nowarn "25"

// for my code, the head should always be the least significant

open Util
open System
open System.Net

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
      let (functionCode :: [errorCode]) = pdu
      let functionCode =
        match functionCode with
        | x when x &&& 0x80uy = 0x80uy -> x - 0x80uy |> Ok
        | x -> "Error flag not set" |> FormatException |> Error

      let functionCode =
        match functionCode with
        | Ok x ->
          FunctionCode.TryFromByte x
          |> function
             | Error _ -> "Function code invalid" |> FormatException |> Error
             | Ok x -> x |> Ok
        | Error x -> x |> Error

      let errorCode = ExceptionCode.TryFromByte errorCode
      match functionCode,errorCode with
      | Error e1, Error _ ->
        sprintf "%s, Exception code invalid" e1.Message |> FormatException |> raise
      | Error e, _ ->
        e |> raise
      | _, Error _ ->
        "Exception code invalid" |> FormatException |> raise
      | Ok functionCode, Ok errorCode ->
        {
          FunctionCode = functionCode
          ExceptionCode = errorCode
        } |> Ok
    with | e ->
      (pdu, e) |> Error

type ReqOffQuant =
  {
    Address : System.UInt16
    Quantity: System.UInt16
  }
  static member TryParse (pdu : PDU) : Result<ReqOffQuant,PDU * exn> =
    try
      let (functionCode :: addressHigh :: addressLow :: quantityHigh :: [quantityLow]) = pdu // throw exception if not an exact match
      {
        Address = [addressLow; addressHigh] |> tU16
        Quantity = [quantityLow; quantityHigh] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.PartialSerialize () =
    let [addressLow; addressHigh] = x.Address |> Util.U16ToBytes
    let [quantityLow; quantityHigh] = x.Quantity |> Util.U16ToBytes
    let functionCode = FunctionCode.ReadDO.ToByte()
    [addressHigh; addressLow; quantityHigh; quantityLow]

type WriteDoRequest =
  {
    Address : System.UInt16
    Value : bool
  }
  static member TryParse (pdu : byte list) : Result<WriteDoRequest, PDU * exn > =
    try
      let (functionCode :: addressHigh :: addressLow :: valueHigh :: [valueLow]) = pdu // throw exception if not an exact match
      {
        Address = [addressLow; addressHigh] |> tU16
        Value = [valueLow; valueHigh] |> tU16 > 0us
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.Serialize () =
    let [addressLow; addressHigh] = x.Address |> Util.U16ToBytes
    let [valL; valH] = x.Value |> Util.BoolToUint16 |> Util.U16ToBytes
    let functionCode = FunctionCode.WriteDO.ToByte()
    [functionCode; addressHigh; addressLow; valH; valL]

type WriteRegRequest =
  {
    Address : System.UInt16
    Value : System.UInt16
  }
  static member TryParse (pdu : byte list) : Result<WriteRegRequest, PDU * exn> =
    try
      let (functionCode :: addressHigh :: addressLow :: valueHigh :: [valueLow]) = pdu // throw exception if not an exact match
      {
        Address = [addressLow; addressHigh] |> tU16
        Value = [valueLow; valueHigh] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error
  member x.Serialize () =
    let [addressLow; addressHigh] = x.Address |> Util.U16ToBytes
    let [valL; valH] = x.Value |> Util.U16ToBytes
    let functionCode = FunctionCode.WriteReg.ToByte()
    [functionCode; addressHigh; addressLow; valH; valL]

type WriteDosRequest =
  {
    Address : System.UInt16
    Quantity: System.UInt16
    // cannot be inferred, this information is needed
    // by the response
    // ByteCount: byte // useful for parsing validation, though
    Values: bool list
  }
  static member TryParse (pdu : byte list) : Result<WriteDosRequest, PDU * exn> =
    try
      let (functionCode :: addressHigh :: addressLow :: quantityHigh :: quantityLow :: byteCount :: values) = pdu // throw exception if not an exact match
      {
        Address = [addressLow; addressHigh] |> tU16
        Quantity = [quantityLow; quantityHigh] |> tU16
        Values = values |> Util.bytesToBool
      } |> Ok
    with | e -> (pdu, e) |> Error
    member x.Serialize () =
      let [addressLow; addressHigh] = x.Address |> Util.U16ToBytes
      let [quantityLow; quantityHigh] = x.Quantity |> Util.U16ToBytes
      let data = x.Values |> Util.BoolsToBytes
      let byteCount = data |> List.length |> byte
      let functionCode = FunctionCode.WriteDOs.ToByte()
      [functionCode; addressHigh; addressLow; quantityHigh; quantityLow; byteCount] @ data

type WriteRegsRequest =
  {
    Address : System.UInt16
    Quantity : System.UInt16
    //ByteCount: byte
    Values : System.UInt16 list
  }
  static member TryParse (pdu : byte list) : Result<WriteRegsRequest,PDU * exn> =
    try
      let (functionCode :: addressHigh :: addressLow :: quantityHigh :: quantityLow :: count :: values) = pdu // throw exception if not an exact match
      if values.Length % 2 <> 0 then
        "Even number of data bytes expected" |> FormatException |> raise

      {
        Address = [addressLow; addressHigh] |> tU16
        Quantity = [quantityLow; quantityHigh] |> tU16
        Values = values |> swapU16s |> Util.bytesToUint16
      } |> Ok
    with | e -> (pdu, e) |> Error
    member x.Serialize () =
      let [addressLow; addressHigh] = x.Address |> Util.U16ToBytes
      let [quantityLow; quantityHigh] = x.Quantity |> Util.U16ToBytes
      let data = x.Values |> Util.U16sToBytes |> Util.swapU16s
      let byteCount = data |> List.length |> byte
      let functionCode = FunctionCode.WriteRegs.ToByte()
      [functionCode; addressHigh; addressLow; quantityHigh; quantityLow; byteCount] @ data

type RtuRequest =
  | ReadDOReq of ReqOffQuant
  | ReadDIReq of ReqOffQuant
  | ReadHRegReq of ReqOffQuant
  | ReadIRegReq of ReqOffQuant
  | WriteDOReq of WriteDoRequest
  | WriteRegReq of WriteRegRequest
  | WriteDOsReq of WriteDosRequest
  | WriteRegsReq of WriteRegsRequest

  static member TryParse (pdu : byte list) : Result<RtuRequest, PDU > =
    let (functionCode :: addressHigh :: addressLow :: remainder) = pdu
    let functionCode = functionCode |> FunctionCode.TryFromByte
    match functionCode with
    | Ok functionCode ->
      match functionCode with
      | FunctionCode.ReadDO ->
        ReqOffQuant.TryParse pdu
        |> function
           | Ok x -> x |> ReadDOReq |> Ok
           | Error (p,e) -> pdu |> Error

      | FunctionCode.ReadDI ->
        ReqOffQuant.TryParse pdu
        |> function
           | Ok x -> x |> ReadDIReq |> Ok
           | Error (p,e) -> pdu |> Error

      | FunctionCode.ReadHReg ->
        ReqOffQuant.TryParse pdu
        |> function
           | Ok x -> x |> ReadHRegReq |> Ok
           | Error (p,e) -> pdu |> Error

      | FunctionCode.ReadIReg ->
        ReqOffQuant.TryParse pdu
        |> function
           | Ok x -> x |> ReadIRegReq |> Ok
           | Error (p,e) -> pdu |> Error

      | FunctionCode.WriteDO ->
        WriteDoRequest.TryParse pdu
        |> function
           | Ok x -> x |> WriteDOReq |> Ok
           | Error (p,e) -> pdu |> Error

      | FunctionCode.WriteReg ->
        WriteRegRequest.TryParse pdu
        |> function
           | Ok x -> x |> WriteRegReq |> Ok
           | Error (p,e) -> pdu |> Error

      | FunctionCode.WriteDOs ->
        WriteDosRequest.TryParse pdu
        |> function
           | Ok x -> x |> WriteDOsReq |> Ok
           | Error (p,e) -> pdu |> Error

      | FunctionCode.WriteRegs ->
        WriteRegsRequest.TryParse pdu
        |> function
           | Ok x -> x |> WriteRegsReq |> Ok
           | Error (p,e) -> pdu |> Error

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
      let (functionCode :: count :: data) = pdu // throw exception if not an exact match
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
      let (functionCode :: count :: data) = pdu // throw exception if not an exact match
      {
        Values = data |> bytesToUint16
      } |> Ok
    with | e -> (pdu, e) |> Error


type WriteDoResponse =
  {
    Address: UInt16
    Value: bool
  }
  member x.Serialize ()  : byte list =
    let oBytes = x.Address |> U16ToBytes |> swapU16s
    let oVal = x.Value |> BoolToUint16 |> U16ToBytes |> swapU16s
    let functionCode = FunctionCode.WriteDO.ToByte()
    functionCode :: oBytes @ oVal

  static member TryParse (pdu : PDU) : Result<WriteDoResponse, PDU * exn>  =
    try
      let (functionCode :: addressHigh :: addressLow :: valueHigh :: [valueLow]) = pdu // throw exception if not an exact match
      {
        Address = [addressHigh; addressLow] |> tU16
        Value = valueHigh = 255uy
      } |> Ok

    with | e -> (pdu, e) |> Error


type WriteRegResponse =
  {
    Address: UInt16
    Value: UInt16
  }
  member x.Serialize ()  : byte list =
    let oBytes = x.Address |> Util.U16ToBytes |> swapU16s
    let oVal = x.Value |> Util.U16ToBytes |> swapU16s
    let functionCode = FunctionCode.WriteReg.ToByte()
    functionCode :: oBytes @ oVal

  static member TryParse (pdu : PDU ) : Result<WriteRegResponse, PDU * exn> =
    try
      let [functionCode; addressHigh; addressLow; valueHigh; valueLow] = pdu
      {
        Address = [addressHigh; addressLow] |> tU16
        Value = [valueHigh; valueLow] |> tU16
      } |> Ok

    with | e -> (pdu, e) |> Error

type ResOffQuant =
  {
    Address: UInt16
    Quantity: UInt16
  }
  member x.PartialSerialize ()  : byte list =
    let oAddress = x.Address |> Util.U16ToBytes |> swapU16s
    let oQuantity = x.Quantity |> Util.U16ToBytes |> swapU16s
    oAddress @ oQuantity
  static member TryParse (pdu : PDU) : Result<ResOffQuant, PDU * exn> =
    try
      let [functionCode; addressHigh; addressLow; quantityHigh; quantityLow] = pdu
      {
        Address = [addressHigh; addressLow] |> tU16
        Quantity = [quantityHigh; quantityLow] |> tU16
      } |> Ok
    with | e -> (pdu, e) |> Error

type RtuResponse =
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

  static member TryParse (pdu : byte list) : Result<RtuResponse, PDU > =
    let (functionCode :: remainder) = pdu
    let isError = functionCode &&& 0x80uy = 0x80uy

    match isError with
    | true ->
      ModError.TryParse(pdu)
      |> function
         | Ok x -> x |> ModErrorRes |> Ok
         | Error (x,_) -> x |> Error
    | false ->
      let functionCode = functionCode |> FunctionCode.TryFromByte

      match functionCode with
      | Ok functionCode ->
        match functionCode with
        | FunctionCode.ReadDO ->
          ResBools.TryParse pdu
          |> function
             | Ok x -> x |> ReadDORes |> Ok
             | Error _ -> pdu |> Error

        | FunctionCode.ReadDI ->
          ResBools.TryParse pdu
          |> function
             | Ok x -> x |> ReadDIRes |> Ok
             | Error _ -> pdu |> Error

        | FunctionCode.ReadHReg ->
          ResRegs.TryParse pdu
          |> function
             | Ok x -> x |> ReadHRegRes |> Ok
             | Error _ -> pdu |> Error

        | FunctionCode.ReadIReg ->
          ResRegs.TryParse pdu
          |> function
             | Ok x -> x |> ReadIRegRes |> Ok
             | Error _ -> pdu |> Error

        | FunctionCode.WriteDO ->
          WriteDoResponse.TryParse pdu
          |> function
             | Ok x -> x |> WriteDORes |> Ok
             | _ -> pdu |> Error

        | FunctionCode.WriteReg ->
          WriteRegResponse.TryParse pdu
          |> function
             | Ok x -> x |> WriteRegRes |> Ok
             | _ -> pdu |> Error

        | FunctionCode.WriteDOs ->
          ResOffQuant.TryParse pdu
          |> function
             | Ok x -> x |> WriteDOsRes |> Ok
             | _ -> pdu |> Error

        | FunctionCode.WriteRegs ->
          ResOffQuant.TryParse pdu
          |> function
             | Ok x -> x |> WriteRegsRes |> Ok
             | _ -> pdu |> Error
      | Error _ -> pdu |> Error


type MbapReq =
  {
    TransactionIdentifier : TransactionIdentifier
    ProtocolIdentifier : ProtocolIdentifier
    Length : Length
    UnitIdentifier : UnitIdentifier
    Request: RtuRequest
  }
  member x.Serialize ()  : byte list =
    // The transaction and protocol identifiers are reversed
    let ti = x.TransactionIdentifier |> U16ToBytes |> swapU16s
    let pi = x.ProtocolIdentifier |> U16ToBytes |> swapU16s
    let len = x.Length |> U16ToBytes |> swapU16s
    let uid = x.UnitIdentifier
    let req = x.Request.Serialize ()

    ti @ pi @ len @ [uid] @ req
  static member TryParse (frame : byte list) : Result<MbapReq, byte list * exn> =
    try
      // the smallest frame is 12 bytes long
      if frame.Length < 12 then
        FormatException("Frame length less than the minimum of 12") |> raise

      let (
           tranIdHigh
        :: tranIdLow
        :: protocolHigh
        :: protocolLow
        :: lengthHigh
        :: lengthLow
        :: unitIdentifier
        :: modPdu) = frame

      let transactionIdentifier = [tranIdLow; tranIdHigh] |> tU16
      let protocol = [protocolLow; protocolHigh] |> tU16
      let length = [lengthLow; lengthHigh; 0uy; 0uy] |> tI32
      let lengthU = [lengthLow; lengthHigh] |> tU16

      // verify the length
      if (frame.Length - 6) <> length then
        FormatException("Frame is reporting a different length than received") |> raise

      if protocolLow <> 0uy || protocolHigh <> 0uy then
        FormatException("Protocol identifier is expected to be zero") |> raise

      let modReq = RtuRequest.TryParse modPdu

      match modReq with
      | Error _ -> (frame, (exn "Unable to parse modbus pdu")) |> Error
      | Ok m ->
          {
            TransactionIdentifier = transactionIdentifier
            ProtocolIdentifier = protocol
            Length = lengthU
            UnitIdentifier = unitIdentifier
            Request = m
          } |> Ok

    with | e -> (frame, e) |> Error

type MbapRes =
  {
    TransactionIdentifier : TransactionIdentifier
    ProtocolIdentifier : ProtocolIdentifier
    UnitIdentifier : UnitIdentifier
    Response : RtuResponse
  }
  member x.Serialize ()  : byte list =
    // The transaction and protocol identifiers are reversed
    let ti = x.TransactionIdentifier |> U16ToBytes |> swapU16s
    let pi = x.ProtocolIdentifier |> U16ToBytes |> swapU16s
    let uid = x.UnitIdentifier
    let res = x.Response.Serialize ()
    let len =
      res
      |> List.length
      |> (+) 1
      |> Convert.ToUInt16
      |> U16ToBytes
      |> swapU16s

    ti @ pi @ len @ [uid] @ res

  static member TryParse (frame : byte list) : Result<MbapRes, byte list * exn> =
    try
      // the smallest frame is 12 bytes long
      if frame.Length < 12 then
        FormatException("Frame length less than the minimum of 12") |> raise

      let (
           tranIdHigh
        :: tranIdLow
        :: protocolHigh
        :: protocolLow
        :: lengthHigh
        :: lengthLow
        :: unitIdentifier
        :: modPdu) = frame

      let transactionIdentifier = [tranIdLow; tranIdHigh] |> tU16
      let protocol = [protocolLow; protocolHigh] |> tU16
      let length = [lengthLow; lengthHigh; 0uy; 0uy] |> tI32
      let lengthU = [lengthLow; lengthHigh] |> tU16

      // verify the length
      if (frame.Length - 6) <> length then
        FormatException("Frame is reporting a different length than received") |> raise

      if protocolLow <> 0uy || protocolHigh <> 0uy then
        FormatException("Protocol identifier is expected to be zero") |> raise

      let modRes = RtuResponse.TryParse modPdu

      match modRes with
      | Error _ -> (frame, (exn "Unable to parse modbus pdu")) |> Error
      | Ok m ->
          {
            TransactionIdentifier = transactionIdentifier
            ProtocolIdentifier = protocol
            UnitIdentifier = unitIdentifier
            Response = m
          } |> Ok

    with | e -> (frame, e) |> Error

type MbapFunc = MbapReq -> Result<MbapRes, unit>
// this is weak, instead of Error of unit, should be error of exception or something
// todo: improve this ^^^

// delegates that are called for each type of function
type ModFunc = RtuRequest -> RtuResponse
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
      let readDOFunc (req : ReqOffQuant) : RtuResponse =
        {
          Status = expected req.Quantity
        } |> ReadDORes
      let readDIFunc (req : ReqOffQuant) : RtuResponse =
        {
          Status = expected req.Quantity
        } |> ReadDIRes
      let readHRegFunc (req : ReqOffQuant) : RtuResponse =
        {
          Values = expected16 req.Quantity
        } |> ReadHRegRes
      let readIRegFunc (req : ReqOffQuant) : RtuResponse =
        {
          Values = expected16 req.Quantity
        } |> ReadIRegRes
      let writeDOFunc (req : WriteDoRequest) : RtuResponse =
        let r : WriteDoResponse =
          {
            Address = req.Address
            Value = req.Value
          }
        r |> WriteDORes
      let writeRegFunc (req : WriteRegRequest) : RtuResponse =
        {
          Address = req.Address
          Value = req.Value
        } |> WriteRegRes

      let writeDOsFunc (req : WriteDosRequest) : RtuResponse =
        {
          Address = req.Address
          Quantity = req.Quantity
        } |> WriteDOsRes

      let writeRegsFunc (req : WriteRegsRequest) : RtuResponse =
        {
          Address = req.Address
          Quantity = req.Quantity
        } |> WriteRegsRes

  // the defaults do nothing, all they do is return is
  // the expected corresponding successful response
  let defaultSuccesses =
    let r (req : RtuRequest) : RtuResponse =
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

type Port = System.UInt32

type ModbusServerConf =
  {
    Port : Port
    IPAddress : IPAddress
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
          } |> Ok
        | _ -> Error str
      | _ -> Error str
    | _ -> Error str

type Hostname = String

type ServerConnection =
  | IPAddress' of IPAddress
  | Hostname of Hostname


type ModbusClientConf =
  {
    Server : ServerConnection
    Port : Port
    SlaveId : int
  }
  static member TryParse (str : string) : Result<ModbusClientConf, string> =
    // string expected in the format tcp://127.0.0.1:502:1
    let str = str.ToLower()
    match str.IndexOf("tcp://") with
    | 0 ->
      let str1 = str.Split("tcp://") |> Array.last
      match str1.Split(":") with
      | [|addr; port; s|] ->
        let resIP = ref (System.Net.IPAddress(0L))
        let successIP = System.Net.IPAddress.TryParse(addr, resIP)
        let resPort = ref 0u
        let successPort = System.UInt32.TryParse(port, resPort)
        let slaveId = ref 0
        let successSlave = System.Int32.TryParse(s, slaveId)
        match successIP, successPort, successSlave with
        | true, true, true ->
          {
            Port = resPort.Value
            Server = resIP.Value |> IPAddress'
            SlaveId = slaveId.Value
          } |> Ok
        | false, true, true ->
          {
            Port = resPort.Value
            Server = addr |> Hostname
            SlaveId = slaveId.Value
          } |> Ok
        | _ -> Error str
      | _ -> Error str
    | _ -> Error str

type Offset = UInt16

// reading of individual addresses should be discouraged
// ideally block-reading of addresses will be followed
// thus i've skipped the single function codes for now
type ModbusClient =
  {
    ReadDOs : Ch<ReqOffQuant * IVar<Result<bool list, exn>>>
    ReadDIs : Ch<ReqOffQuant * IVar<Result<bool list, exn>>>
    ReadHRegs: Ch<ReqOffQuant * IVar<Result<UInt16 list, exn>>>
    ReadIRegs: Ch<ReqOffQuant * IVar<Result<UInt16 list, exn>>>
    WriteDOs: Ch<(Offset * bool list) * IVar<Result<unit, exn>>>
    WriteRegs: Ch<(Offset * UInt16 list) * IVar<Result<unit, exn>>>
  }