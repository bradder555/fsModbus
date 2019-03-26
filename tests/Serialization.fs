module Test.Serialization

open Hopac
open Expecto
open ModbusTypes

let tests = 
    testList "Serialization" [
        test "ModError" {
            Expect.equal 1 1 "should be one"
        }
    ]