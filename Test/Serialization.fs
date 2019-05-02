module Test.Serialization

open Expecto
open ModbusTypes
open Util

let tests =
    testList "Serialization" [
        testList "ReadDOError" [ // this is simple, i'm satisfied with just a couple tests
            test "IllegalDataAddress" {
                let t = {
                    FunctionCode = ReadDO
                    ExceptionCode = IllegalDataAddress
                }

                Expect.equal
                  (t.Serialize ())
                  [(128uy + 1uy); 2uy]
                  "[129, 2] expected"
            }
            test "SlaveDeviceFailure" {
                let t = {
                    FunctionCode = ReadDO
                    ExceptionCode = SlaveDeviceFailure
                }

                Expect.equal
                  (t.Serialize ())
                  [(128uy + 1uy); 4uy]
                  "[129, 4] expected"
            }
            test "SlaveDeviceBusy" {
                let t = {
                    FunctionCode = ReadDO
                    ExceptionCode = SlaveDeviceBusy
                }

                Expect.equal
                  (t.Serialize ())
                  [(128uy + 1uy); 6uy]
                  "[129, 6] expected"
            }
        ]
        testList "WriteHRegError" [
            test "IllegalDataAddress" {
                let t = {
                    FunctionCode = WriteReg
                    ExceptionCode = IllegalDataAddress
                }

                Expect.equal
                  (t.Serialize ())
                  [(128uy + 6uy); 2uy]
                  "[134, 2] expected"
            }
            test "SlaveDeviceFailure" {
                let t = {
                    FunctionCode = WriteReg
                    ExceptionCode = SlaveDeviceFailure
                }

                Expect.equal
                  (t.Serialize ())
                  [(128uy + 6uy); 4uy]
                  "[134, 4] expected"
            }
            test "SlaveDeviceBusy" {
                let t = {
                    FunctionCode = WriteReg
                    ExceptionCode = SlaveDeviceBusy
                }

                Expect.equal
                  (t.Serialize ())
                  [(128uy + 6uy); 6uy]
                  "[134, 6] expected"
            }
        ]
        testList "WriteDoRequest" [
            test "LSB Only true" {
                let t : WriteDoRequest = {
                    Address = 32us
                    Value = true
                }
                Expect.equal
                  (t.Serialize ())
                  [5uy; 0uy; 32uy; 0xFFuy; 00uy]
                  "[5uy; 0uy; 32uy; 0xFFuy; 0uy] "
            }
            test "LSB Only false" {
                let t : WriteDoRequest = {
                    Address = 32us
                    Value = false
                }
                Expect.equal
                  (t.Serialize ())
                  [5uy; 0uy; 32uy; 0x00uy; 00uy]
                  "[5uy; 0uy; 32uy; 0x00uy; 0uy] "
            }
            test "LSB&MSB true" {
                let t : WriteDoRequest = {
                    Address = 512us + 32us
                    Value = true
                }
                Expect.equal
                  (t.Serialize ())
                  [5uy; 2uy; 32uy; 0xFFuy; 00uy]
                  "[5uy; 2uy; 32uy; 0xFFuy; 0uy] "
            }
        ]
        testList "WriteRegRequest" [
            test "LSB Only" {
                let t : WriteRegRequest = {
                    Address = (0x00us <<< 8) + 0x32us
                    Value = (0x00us <<< 8) + 0x55us
                }
                Expect.equal
                  (t.Serialize ())
                  [6uy; 0uy; 0x32uy; 0x00uy; 0x55uy]
                  "[6uy; 0uy; 32uy; 0xFFuy; 0uy] "
            }
            test "LSBs & MSB" {
                let t : WriteRegRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Value = (0xA9us <<< 8) + 0x55us
                }
                Expect.equal
                  (t.Serialize ())
                  [6uy; 0x66uy; 0x32uy; 0xA9uy; 0x55uy]
                  "[6uy; 0uy; 32uy; 0x00uy; 0uy] "
            }
        ]

        testList "WriteDosRequest" [
            test "6 values" {
                let t : WriteDosRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Quantity = (0x00us <<< 8) + 06us
                    Values = [true; true; true; true; false; false; false; false;]
                }
                Expect.equal
                  (t.Serialize ())
                  [15uy; 0x66uy; 0x32uy; 0x00uy; 0x06uy; 1uy; 15uy]
                  "[15uy; 0x66uy; 0x32uy; 0x00uy; 0x06uy; 1uy; 15uy]"
            }
            test "8 values" {
                let t : WriteDosRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Quantity = (0x00us <<< 8) + 08us
                    Values = [true; true; true; true; false; false; false; true;]
                }
                Expect.equal
                  (t.Serialize ())
                  [15uy; 0x66uy; 0x32uy; 0x00uy; 0x08uy; 1uy; 143uy]
                  "[15uy; 0x66uy; 0x32uy; 0x00uy; 0x08uy; 1uy; 143uy]"
            }
            test "12 values" {
                let t : WriteDosRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Quantity = (0x00us <<< 8) + 12us
                    Values =
                      [
                        true;
                        true;
                        true;
                        true;
                        false;
                        false;
                        false;
                        true;
                        // ---
                        true;
                        true;
                        false;
                        false;
                        false;
                        false;
                        false;
                        false;
                        ]
                }
                Expect.equal
                  (t.Serialize ())
                  [15uy; 0x66uy; 0x32uy; 0x00uy; 12uy; 2uy; 143uy; 3uy]
                  "[15uy; 0x66uy; 0x32uy; 0x00uy; 12uy; 2uy; 143uy; 3uy]"
            }
            test "20 values" {
                let t : WriteDosRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Quantity = (0x00us <<< 8) + 20us
                    Values =
                      [
                        true;
                        true;
                        true;
                        true;
                        false;
                        false;
                        false;
                        true;
                        // ---
                        true;
                        true;
                        false;
                        false;
                        false;
                        false;
                        false;
                        false;
                        // ---
                        true;
                        true;
                        true;
                        true;
                        false;
                        false;
                        false;
                        false;
                        ]
                }
                Expect.equal
                  (t.Serialize ())
                  [15uy; 0x66uy; 0x32uy; 0x00uy; 20uy; 3uy; 143uy; 3uy; 15uy;]
                  "[15uy; 0x66uy; 0x32uy; 0x00uy; 20uy; 3uy; 143uy; 3uy; ]"
            }
            test "240 values" {
                let t : WriteDosRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Quantity = 1920us
                    Values = [0..1919] |> List.map (fun x -> x % 2 = 0)
                }
                let expectedData = [1..(240)] |> List.map (fun _ -> 85uy)
                // 1920 = 07 80

                Expect.equal
                  (t.Serialize ())
                  ([15uy; 0x66uy; 0x32uy; 0x07uy; 0x80uy; 240uy; ] @ expectedData)
                  "[15uy; 0x66uy; 0x32uy; 0x07uy; 0x80uy; 240uy; ...]"
            }
        ]

        testList "WriteRegsRequest" [
            test "1 value" {
                let t : WriteRegsRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Quantity = (0x00us <<< 8) + 01us
                    Values = [1us]
                }
                Expect.equal
                  (t.Serialize ())
                  [16uy; 0x66uy; 0x32uy; 0x00uy; 0x01uy; 2uy; 0uy; 1uy]
                  "[16uy; 0x66uy; 0x32uy; 0x00uy; 0x01uy; 2uy; 0uy; 1uy]"
            }
            test "2 large values" {
                let t : WriteRegsRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Quantity = (0x00us <<< 8) + 02us
                    Values = [33003us; 18711us] // head = least sig
                    // 80 EB 49 17
                }
                Expect.equal
                  (t.Serialize ())
                  [16uy; 0x66uy; 0x32uy; 0x00uy; 0x02uy; 4uy; 0x80uy; 0xEBuy; 0x49uy; 0x17uy]
                  "[16uy; 0x66uy; 0x32uy; 0x00uy; 0x01uy; 2uy; ...]"
            }
            test "120 values" {
                let v = [1us..120us] |> List.map ((*) 200us) // head = least sig
                let t : WriteRegsRequest = {
                    Address = (0x66us <<< 8) + 0x32us
                    Quantity = (0x00us <<< 8) + 120us
                    Values = v
                }
                let expectedData = v |> U16sToBytes |> swapU16s
                Expect.equal
                  (t.Serialize ())
                  ([16uy; 0x66uy; 0x32uy; 0uy; 120uy; 240uy] @ expectedData)
                  "[16uy; 0x66uy; 0x32uy; 0uy; 120uy; 240uy;...]"
            }
        ]

        testList "WriteDoResponse" [
            // the client needs to keep track
            // of the transaction, the Address and requested quantity
            // so much complexity to save a few bytes!
            test "true" {
                let t : WriteDoResponse = {
                    Address = 0x20us
                    Value = true
                }

                let expected =
                  [
                    5uy;
                    0x00uy;
                    0x20uy;
                    0xffuy;
                    0x00uy;
                  ]

                Expect.equal
                  (t.Serialize ())
                  expected
                  "[5uy; 00uy; 20uy; 0xffuy; 00uy]"
            }

            test "false" {
                let t : WriteDoResponse = {
                    Address = 0x20us
                    Value = false
                }

                let expected =
                  [
                    5uy;
                    0x00uy;
                    0x20uy;
                    0x00uy;
                    0x00uy;
                  ]

                Expect.equal
                  (t.Serialize ())
                  expected
                  "[5uy; 00uy; 20uy; 0x00uy; 00uy]"
            }

            test "true high address" {
                let t : WriteDoResponse = {
                    Address = 0x2120us
                    Value = true
                }

                let expected =
                  [
                    5uy;
                    0x21uy;
                    0x20uy;
                    0xffuy;
                    0x00uy;
                  ]

                Expect.equal
                  (t.Serialize ())
                  expected
                  "[5uy; 0x21uy; 0x20uy; 0xffuy; 00uy]"
            }
        ]

        testList "WriteRegResponse" [
            // the client needs to keep track
            // of the transaction, the Address and requested quantity
            // so much complexity to save a few bytes!
            test "high Address and value" {
                let t : WriteRegResponse = {
                    Address = 0x3D20us
                    Value = 0xABCDus
                }

                let expected =
                  [
                    6uy;
                    0x3Duy;
                    0x20uy;
                    0xABuy;
                    0xCDuy;
                  ]

                Expect.equal
                  (t.Serialize ())
                  expected
                  "[6uy; 3Duy; 20uy; 0xABuy; 0xCDuy]"
            }

        ]
    ]
