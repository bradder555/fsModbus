module ModbusTypes
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

type ErrorCode = 
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
    ErrorCode : ErrorCode
  }
  member x.serialize () =
    let exCode = x.FunctionCode.ToByte() + 128uy
    let erCode = x.ErrorCode.ToByte() 
    [exCode; erCode]      

type ReadDoRequest = 
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member tryParse (pdu : PDU) : ReadDoRequest = 
    let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
    {
      Offset = [addrL; addrH] |> tU16
      Quantity = [countL; countH] |> tU16
    }

type ReadDiRequest = 
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member tryParse (pdu : PDU) : ReadDiRequest = 
    let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
    {
      Offset = [addrL; addrH] |> tU16
      Quantity = [countL; countH] |> tU16
    }

type ReadHRegRequest = 
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member tryParse (pdu : byte list) : ReadHRegRequest = 
    let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
    {
      Offset = [addrL; addrH] |> tU16
      Quantity = [countL; countH] |> tU16
    }

type ReadIRegRequest = 
  {
    Offset : System.UInt16
    Quantity: System.UInt16
  }
  static member tryParse (pdu : byte list) : ReadIRegRequest = 
    let (fc :: addrH :: addrL :: countH :: countL :: []) = pdu // throw exception if not an exact match
    {
      Offset = [addrL; addrH] |> tU16
      Quantity = [countL; countH] |> tU16
    }

type WriteDoRequest = 
  {
    Offset : System.UInt16
    Value : bool
  }
  static member tryParse (pdu : byte list) : WriteDoRequest = 
    let (fc :: addrH :: addrL :: valueH :: valueL :: []) = pdu // throw exception if not an exact match
    {
      Offset = [addrL; addrH] |> tU16
      Value = [valueL; valueH] |> tU16 > 0us
    }

type WriteRegRequest = 
  {
    Offset : System.UInt16
    Value : System.UInt16
  }
  static member tryParse (pdu : byte list) : WriteRegRequest = 
    let (fc :: addrH :: addrL :: valueH :: valueL :: []) = pdu // throw exception if not an exact match
    {
      Offset = [addrL; addrH] |> tU16
      Value = [valueL; valueH] |> tU16
    }

type WriteDosRequest = 
  {
    Offset : System.UInt16
    Quantity: System.UInt16
    ByteCount: byte
    Values: bool list
  }
  static member tryParse (pdu : byte list) : WriteDosRequest = 
    let (fc :: addrH :: addrL :: countH :: countL :: count :: values) = pdu // throw exception if not an exact match
    {
      Offset = [addrL; addrH] |> tU16
      Quantity = [countL; countH] |> tU16
      ByteCount = count
      Values = values |> Util.bytesToBool
    }  

type WriteRegsRequest = 
  {
    Offset : System.UInt16
    Quantity : System.UInt16
    ByteCount: byte
    Values : System.UInt16 list
  }
  static member tryParse (pdu : byte list) : WriteRegsRequest = 
    let (fc :: addrH :: addrL :: countH :: countL :: count :: values) = pdu // throw exception if not an exact match
    if values.Length % 2 <> 0 then
      "Even number of data bytes expected" |> FormatException |> raise

    {
      Offset = [addrL; addrH] |> tU16
      Quantity = [countL; countH] |> tU16
      ByteCount = count
      Values = values |> Util.bytesToUint16
    }    

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
  member x.serialize () : byte list = 
    let status = x.Status |> Util.BoolsToBytes
    let c = status |> List.length |> byte
    [1uy; c] @ status

type ReadDiResponse = 
  {
    Status: bool list
  }
  member x.serialize () : byte list = 
    let status = x.Status |> Util.BoolsToBytes
    let c = status |> List.length |> byte
    [2uy; c] @ status  

type ReadHRegResponse = 
  {
    Values : UInt16 list
  }
  member x.serialize () : byte list = 
    let count = x.Values |> List.length |> (*)2 |> byte
    let data = x.Values |> Util.U16sToBytes
    [3uy; count] @ data    

type ReadIRegResponse = 
  {
    Values : UInt16 list
  }
  member x.serialize () : byte list = 
    let count = x.Values |> List.length |> (*)2 |> byte
    let data = x.Values |> Util.U16sToBytes
    [4uy; count] @ data    


type WriteDoResponse = 
  {
    Offset: UInt16
    Value: bool
  }
  member x.serialize () : byte list = 
    let oBytes = x.Offset |> Util.U16ToBytes
    let oVal = x.Value |> Util.BoolToUint16 |> Util.U16ToBytes
    [5uy] @ oBytes @ oVal


type WriteRegResponse = 
  {
    Offset: UInt16
    Value: UInt16
  }
  member x.serialize () : byte list = 
    let oBytes = x.Offset |> Util.U16ToBytes
    let oVal = x.Value |> Util.U16ToBytes
    [6uy] @ oBytes @ oVal

type WriteDosResponse = 
  {
    Offset: UInt16
    Count: UInt16
  }
  member x.serialize () : byte list = 
    let oOffset = x.Offset |> Util.U16ToBytes |> List.rev
    let oCount = x.Count |> Util.U16ToBytes |> List.rev
    [15uy] @ oOffset @ oCount 

type WriteRegsResponse = 
  {
    Offset: UInt16
    Count: UInt16
  }
  member x.serialize () : byte list = 
    let oOffset = x.Offset |> Util.U16ToBytes
    let oCount = x.Count |> Util.U16ToBytes
    [16uy] @ oOffset @ oCount   

  static member TryFromByte (x : byte) : Result<ErrorCode,byte> =
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
  member x.serialize () : byte list = 
    match x with
    | ReadDORes y    -> y.serialize ()
    | ReadDIRes y    -> y.serialize ()
    | ReadHRegRes y  -> y.serialize ()
    | ReadIRegRes y  -> y.serialize ()
    | WriteDORes y   -> y.serialize ()
    | WriteRegRes y  -> y.serialize ()
    | WriteDOsRes y  -> y.serialize ()
    | WriteRegsRes y -> y.serialize ()
    | ModErrorRes y -> y.serialize ()

type Mbap = 
  {
    TransactionIdentifier : TransactionIdentifier
    ProtocolIdentifier : ProtocolIdentifier
    Length : Length
    UnitIdentifier : UnitIdentifier
  }
  member x.serialize () : byte list = 
    // The transaction and protocol identifiers are reversed
    let ti = x.TransactionIdentifier |> U16ToBytes |> List.rev
    let pi = x.ProtocolIdentifier |> U16ToBytes |> List.rev
    let len = x.Length |> U16ToBytes |> List.rev
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
    nMbap.serialize() @ pdu @ []

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
  