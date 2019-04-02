module Test.Deserialization

open Expecto
open ModbusTypes

let tests =
    testList "Deserialization" [
        test "ModError" {
            Expect.equal 1 1 "should be one"
        }
    ]