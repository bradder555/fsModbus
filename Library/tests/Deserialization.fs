module Test.Deserialization

open Expecto
open ModbusTypes

let tests =
    testList "Deserialization" [
        test "ModError" {
            let t = ModError.TryParse [0x81uy; 0x02uy]
            let e =
              {
                  FunctionCode = FunctionCode.ReadDO
                  ExceptionCode = ExceptionCode.IllegalDataAddress
              } |> Ok
            Expect.equal t e "should be one"
        }
    ]