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
      (*
      testList "ReadDoRequest" [
        test "parse ok" {
          let payload = [1uy; 2uy; 4uy; 4uy; 1uy]
          let e : ReadDoRequest = {
              Offset = 0x0204us
              Quantity = 0x0401us
            } 

          let e : Result<ReadDoRequest, PDU * exn> = e |> Ok
          
          let t = ReadDoRequest.TryParse payload
          Expect.equal e t "Valid ReadDoRequest "
        }


        test "parse fail short" {
          let payload = [1uy; 2uy; 4uy;]
          let t = ReadDoRequest.TryParse payload
          Expect.isError t "Should be error"
          
          let (pdu, exT) = 
            t 
            |> function 
               | Result.Error x ->  x 
               | _ -> [], Exception()
          
          Expect.equal pdu payload "the pdu should be returned"
          
          let exT = exT.GetType()
          let exT1 = MatchFailureException("",0,0).GetType()
          Expect.equal exT exT1 "We are expecting a Match Failure Exception"
        }

        test "parse fail long" {
          let payload = [1uy; 2uy; 4uy; 2uy; 2uy; 5uy]
          let t = ReadDoRequest.TryParse payload
          Expect.isError t "Should be error"
          
          let (pdu, exT) = 
            t 
            |> function 
               | Result.Error x ->  x 
               | _ -> [], Exception()
          
          Expect.equal pdu payload "the pdu should be returned"
          
          let exT = exT.GetType()
          let exT1 = MatchFailureException("",0,0).GetType()
          Expect.equal exT exT1 "We are expecting a Match Failure Exception"
        }        
      ]

      testList "ReadDiRequest" [
        test "parse ok" {
          let payload = [2uy; 2uy; 4uy; 4uy; 1uy]
          let e : ReadDiRequest = {
              Offset = 0x0204us
              Quantity = 0x0401us
            } 

          let e : Result<ReadDiRequest, PDU * exn> = e |> Ok
          
          let t = ReadDiRequest.TryParse payload
          Expect.equal e t "Valid ReadDoRequest "
        }


        test "parse fail short" {
          let payload = [2uy; 2uy; 4uy;]
          let t = ReadDiRequest.TryParse payload
          Expect.isError t "Should be error"
          
          let (pdu, exT) = 
            t 
            |> function 
               | Result.Error x ->  x 
               | _ -> [], Exception()
          
          Expect.equal pdu payload "the pdu should be returned"
          
          let exT = exT.GetType()
          let exT1 = MatchFailureException("",0,0).GetType()
          Expect.equal exT exT1 "We are expecting a Match Failure Exception"
        }

        test "parse fail long" {
          let payload = [2uy; 2uy; 4uy; 2uy; 2uy; 5uy]
          let t = ReadDiRequest.TryParse payload
          Expect.isError t "Should be error"
          
          let (pdu, exT) = 
            t 
            |> function 
               | Result.Error x ->  x 
               | _ -> [], Exception()
          
          Expect.equal pdu payload "the pdu should be returned"
          
          let exT = exT.GetType()
          let exT1 = MatchFailureException("",0,0).GetType()
          Expect.equal exT exT1 "We are expecting a Match Failure Exception"
        }        
      ]
      
      testList "ReadHRegRequest" [
        test "parse ok" {
          let payload = [3uy; 2uy; 4uy; 4uy; 1uy]
          let e : ReadHRegRequest = {
              Offset = 0x0204us
              Quantity = 0x0401us
            } 

          let e : Result<ReadHRegRequest, PDU * exn> = e |> Ok
          
          let t = ReadHRegRequest.TryParse payload
          Expect.equal e t "Valid ReadDoRequest "
        }


        test "parse fail short" {
          let payload = [3uy; 2uy; 4uy;]
          let t = ReadHRegRequest.TryParse payload
          Expect.isError t "Should be error"
          
          let (pdu, exT) = 
            t 
            |> function 
               | Result.Error x ->  x 
               | _ -> [], Exception()
          
          Expect.equal pdu payload "the pdu should be returned"
          
          let exT = exT.GetType()
          let exT1 = MatchFailureException("",0,0).GetType()
          Expect.equal exT exT1 "We are expecting a Match Failure Exception"
        }

        test "parse fail long" {
          let payload = [3uy; 2uy; 4uy; 2uy; 2uy; 5uy]
          let t = ReadDiRequest.TryParse payload
          Expect.isError t "Should be error"
          
          let (pdu, exT) = 
            t 
            |> function 
               | Result.Error x ->  x 
               | _ -> [], Exception()
          
          Expect.equal pdu payload "the pdu should be returned"
          
          let exT = exT.GetType()
          let exT1 = MatchFailureException("",0,0).GetType()
          Expect.equal exT exT1 "We are expecting a Match Failure Exception"
        }        
      ]

      testList "ReadIRegRequest" [
        test "parse ok" {
          let payload = [4uy; 2uy; 4uy; 4uy; 1uy]
          let e : ReadIRegRequest = {
              Offset = 0x0204us
              Quantity = 0x0401us
            } 

          let e : Result<ReadIRegRequest, PDU * exn> = e |> Ok
          
          let t = ReadIRegRequest.TryParse payload
          Expect.equal e t "Valid ReadDoRequest "
        }


        test "parse fail short" {
          let payload = [4uy; 2uy; 4uy;]
          let t = ReadIRegRequest.TryParse payload
          Expect.isError t "Should be error"
          
          let (pdu, exT) = 
            t 
            |> function 
               | Result.Error x ->  x 
               | _ -> [], Exception()
          
          Expect.equal pdu payload "the pdu should be returned"
          
          let exT = exT.GetType()
          let exT1 = MatchFailureException("",0,0).GetType()
          Expect.equal exT exT1 "We are expecting a Match Failure Exception"
        }

        test "parse fail long" {
          let payload = [4uy; 2uy; 4uy; 2uy; 2uy; 5uy]
          let t = ReadDiRequest.TryParse payload
          Expect.isError t "Should be error"
          
          let (pdu, exT) = 
            t 
            |> function 
               | Result.Error x ->  x 
               | _ -> [], Exception()
          
          Expect.equal pdu payload "the pdu should be returned"
          
          let exT = exT.GetType()
          let exT1 = MatchFailureException("",0,0).GetType()
          Expect.equal exT exT1 "We are expecting a Match Failure Exception"
        }        
      ]
      *)
    ]