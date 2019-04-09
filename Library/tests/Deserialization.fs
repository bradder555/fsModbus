module Test.Deserialization

open System
open Expecto
open ModbusTypes

let tests =
    testList "Deserialization" [
      testList "ModError" [
        test "parses" {
            let t = ModError.TryParse [0x81uy; 0x02uy]
            let e =
              {
                  FunctionCode = FunctionCode.ReadDO
                  ExceptionCode = ExceptionCode.IllegalDataAddress
              } |> Ok
            Expect.equal t e "Should have ReadDO and Illegal Address"
        }
        test "parse fail no error flag" {
            let pdu = [0x01uy; 0x01uy]
            let t = ModError.TryParse pdu
            let t =
              match t with
              | Ok t -> Ok t
              | Error (pdu, e) ->
                let e = e.Message |> FormatException :> exn
                (pdu, e) |> Error
            let ex = "Error flag not set" |> FormatException :> exn
            // Expecto rightfully didn't equate FException1 == FException2
            // Thus format them and compare as strings
            let t = t |> sprintf "%A"
            let e = (pdu, ex) |> Error |> sprintf "%A"
            Expect.equal t e "Should return a 'no error flag' error"
        }
        test "parse fail junk function code" {
            let pdu = [0x89uy; 0x01uy]
            let t = ModError.TryParse pdu
            let t =
              match t with
              | Ok t -> Ok t
              | Error (pdu, e) ->
                let e = e.Message |> FormatException :> exn
                (pdu, e) |> Error
            let ex = "Function code invalid" |> FormatException :> exn
            // Expecto rightfully didn't equate FException1 == FException2
            // Thus format them and compare as strings
            let t = t |> sprintf "%A"
            let e = (pdu, ex) |> Error |> sprintf "%A"
            Expect.equal t e "Should return function code invalid error"
        }
        test "parse complete junk" {
            let pdu = [0x89uy; 0x55uy]
            let t = ModError.TryParse pdu
            let t =
              match t with
              | Ok t -> Ok t
              | Error (pdu, e) ->
                let e = e.Message |> FormatException :> exn
                (pdu, e) |> Error
            let ex = "Function code invalid, Exception code invalid" |> FormatException :> exn
            // Expecto rightfully didn't equate FException1 == FException2
            // Thus format them and compare as strings
            let t = t |> sprintf "%A"
            let e = (pdu, ex) |> Error |> sprintf "%A"
            Expect.equal t e "Should return a 'no error flag' error"
        }
        test "parse long junk" {
            let pdu = [0x89uy; 0x55uy; 0x19uy]
            let t = ModError.TryParse pdu
            let t =
              match t with
              | Ok t -> Ok t
              | Error (pdu, e) ->
                let e = e.Message |> FormatException :> exn
                (pdu, e) |> Error
            let ex = "The match cases were incomplete" |> FormatException :> exn
            // Expecto rightfully didn't equate FException1 == FException2
            // Thus format them and compare as strings
            let t = t |> sprintf "%A"
            let e = (pdu, ex) |> Error |> sprintf "%A"
            Expect.equal t e "Incomplete match case"
        }
      ]
    ]