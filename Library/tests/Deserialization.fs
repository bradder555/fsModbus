module Test.Deserialization

open Hopac
open Expecto
open ModbusTypes

let tests = 
    testList "Deserialization" [
        test "ModError" {
            Expect.equal 1 1 "should be one"
        }
    ]