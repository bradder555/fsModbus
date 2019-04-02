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
        testList "ReadDoRequest" [
            test "LSB Only" {
                let t : ReadDoRequest = {
                    Offset = 32us
                    Quantity = 16us
                }
                Expect.equal
                  (t.Serialize ())
                  [1uy; 0uy; 32uy; 0uy; 16uy]
                  "[1uy; 0uy; 32uy; 0uy; 16uy] "
            }
            test "MSB Only" {
                let t : ReadDoRequest = {
                    Offset = 512us
                    Quantity = 1024us
                }
                Expect.equal
                  (t.Serialize ())
                  [1uy; 2uy; 0uy; 4uy; 0uy]
                  "[1uy; 0uy; 32uy; 0uy; 16uy] "
            }
            test "MSB & LSB" {
                let t : ReadDoRequest = {
                    Offset = 512us + 32us
                    Quantity = 1024us + 16us
                }
                Expect.equal
                  (t.Serialize ())
                  [1uy; 2uy; 32uy; 4uy; 16uy]
                  "[1uy; 0uy; 32uy; 0uy; 16uy] "
            }
        ]
        testList "ReadDiRequest" [
            test "LSB Only" {
                let t : ReadDiRequest = {
                    Offset = 32us
                    Quantity = 16us
                }
                Expect.equal
                  (t.Serialize ())
                  [2uy; 0uy; 32uy; 0uy; 16uy]
                  "[2uy; 0uy; 32uy; 0uy; 16uy] "
            }
            test "MSB Only" {
                let t : ReadDiRequest = {
                    Offset = 512us
                    Quantity = 1024us
                }
                Expect.equal
                  (t.Serialize ())
                  [2uy; 2uy; 0uy; 4uy; 0uy]
                  "[2uy; 0uy; 32uy; 0uy; 16uy] "

            }
            test "MSB & LSB" {
                let t : ReadDiRequest = {
                    Offset = 512us + 32us
                    Quantity = 1024us + 16us
                }
                Expect.equal
                  (t.Serialize ())
                  [2uy; 2uy; 32uy; 4uy; 16uy]
                  "[2uy; 0uy; 32uy; 0uy; 16uy] "
            }
        ]
        testList "ReadHRegRequest" [
            test "LSB Only" {
                let t : ReadHRegRequest = {
                    Offset = 32us
                    Quantity = 16us
                }
                Expect.equal
                  (t.Serialize ())
                  [3uy; 0uy; 32uy; 0uy; 16uy]
                  "[3uy; 0uy; 32uy; 0uy; 16uy] "

            }
            test "MSB Only" {
                let t : ReadHRegRequest = {
                    Offset = 512us
                    Quantity = 1024us
                }
                Expect.equal
                  (t.Serialize ())
                  [3uy; 2uy; 0uy; 4uy; 0uy]
                  "[3uy; 0uy; 32uy; 0uy; 16uy] "
            }
            test "MSB & LSB" {
                let t : ReadHRegRequest = {
                    Offset = 512us + 32us
                    Quantity = 1024us + 16us
                }
                Expect.equal
                  (t.Serialize ())
                  [3uy; 2uy; 32uy; 4uy; 16uy]
                  "[3uy; 0uy; 32uy; 0uy; 16uy] "
            }
        ]
        testList "ReadIRegRequest" [
            test "LSB Only" {
                let t : ReadIRegRequest = {
                    Offset = 32us
                    Quantity = 16us
                }
                Expect.equal
                  (t.Serialize ())
                  [4uy; 0uy; 32uy; 0uy; 16uy]
                  "[4uy; 0uy; 32uy; 0uy; 16uy] "
            }
            test "MSB Only" {
                let t : ReadIRegRequest = {
                    Offset = 512us
                    Quantity = 1024us
                }
                Expect.equal
                  (t.Serialize ())
                  [4uy; 2uy; 0uy; 4uy; 0uy]
                  "[4uy; 0uy; 32uy; 0uy; 16uy] "
            }
            test "MSB & LSB" {
                let t : ReadIRegRequest = {
                    Offset = 512us + 32us
                    Quantity = 1024us + 16us
                }
                Expect.equal
                  (t.Serialize ())
                  [4uy; 2uy; 32uy; 4uy; 16uy]
                  "[4uy; 0uy; 32uy; 0uy; 16uy] "
            }
        ]
        testList "WriteDoRequest" [
            test "LSB Only true" {
                let t : WriteDoRequest = {
                    Offset = 32us
                    Value = true
                }
                Expect.equal
                  (t.Serialize ())
                  [5uy; 0uy; 32uy; 0xFFuy; 00uy]
                  "[5uy; 0uy; 32uy; 0xFFuy; 0uy] "
            }
            test "LSB Only false" {
                let t : WriteDoRequest = {
                    Offset = 32us
                    Value = false
                }
                Expect.equal
                  (t.Serialize ())
                  [5uy; 0uy; 32uy; 0x00uy; 00uy]
                  "[5uy; 0uy; 32uy; 0x00uy; 0uy] "
            }
            test "LSB&MSB true" {
                let t : WriteDoRequest = {
                    Offset = 512us + 32us
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
                    Offset = (0x00us <<< 8) + 0x32us
                    Value = (0x00us <<< 8) + 0x55us
                }
                Expect.equal
                  (t.Serialize ())
                  [6uy; 0uy; 0x32uy; 0x00uy; 0x55uy]
                  "[6uy; 0uy; 32uy; 0xFFuy; 0uy] "
            }
            test "LSBs & MSB" {
                let t : WriteRegRequest = {
                    Offset = (0x66us <<< 8) + 0x32us
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
                    Offset = (0x66us <<< 8) + 0x32us
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
                    Offset = (0x66us <<< 8) + 0x32us
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
                    Offset = (0x66us <<< 8) + 0x32us
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
                    Offset = (0x66us <<< 8) + 0x32us
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
                    Offset = (0x66us <<< 8) + 0x32us
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
                    Offset = (0x66us <<< 8) + 0x32us
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
                    Offset = (0x66us <<< 8) + 0x32us
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
                    Offset = (0x66us <<< 8) + 0x32us
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

        testList "ReadDoResponse" [
            // the client needs to keep track
            // of the transaction, the offset and requested quantity
            // so much complexity to save a few bytes!
            test "1 byte" {
                let t : ReadDoResponse = {
                    Status = [true; true; true; true; false; false; false; false;]
                }
                Expect.equal
                  (t.Serialize ())
                  [1uy; 0x01uy; 15uy]
                  "[1uy; 0x01uy; 15uy]"
            }
            test "3 bytes" {
                let t : ReadDoResponse = {
                    Status =
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
                  [1uy; 0x03uy; 143uy; 3uy; 15uy]
                  "[1uy; 0x03uy; 143uy; 3uy; 15uy]"
            }

        ]
        testList "ReadDiResponse" [
            // the client needs to keep track
            // of the transaction, the offset and requested quantity
            // so much complexity to save a few bytes!
            test "1 byte" {
                let t : ReadDiResponse = {
                    Status = [true; true; true; true; false; false; false; false;]
                }
                Expect.equal
                  (t.Serialize ())
                  [2uy; 0x01uy; 15uy]
                  "[2uy; 0x01uy; 15uy]"
            }

            test "2 bytes" {
                let t : ReadDiResponse = {
                    Status =
                      [
                          true;
                          true;
                          true;
                          true;
                          false;
                          false;
                          false;
                          false;
                          // ---
                          false;
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
                  [2uy; 0x02uy; 15uy; 2uy]
                  "[2uy; 0x02uy; 15uy; 2uy]"
            }

        ]

        testList "ReadHRegResponse" [
            // the client needs to keep track
            // of the transaction, the offset and requested quantity
            // so much complexity to save a few bytes!
            test "10 values" {
                let v1 = [1us..5us]
                let v2 = [512us..516us]
                let t : ReadHRegResponse = {
                    Values = v1 @ v2
                }

                let expected =
                  [
                    3uy;
                    20uy;
                    0uy; 1uy;
                    0uy; 2uy;
                    0uy; 3uy;
                    0uy; 4uy;
                    0uy; 5uy;
                    2uy; 0uy;
                    2uy; 1uy;
                    2uy; 2uy;
                    2uy; 3uy;
                    2uy; 4uy
                  ]

                Expect.equal
                  (t.Serialize ())
                  expected
                  "[3uy; 20uy; 00uy; 01uy...]"
            }


        ]

        testList "ReadIRegResponse" [
            // the client needs to keep track
            // of the transaction, the offset and requested quantity
            // so much complexity to save a few bytes!
            test "10 values" {
                let v1 = [1us..5us]
                let v2 = [512us..516us]
                let t : ReadIRegResponse = {
                    Values = v1 @ v2
                }

                let expected =
                  [
                    4uy;
                    20uy;
                    0uy; 1uy;
                    0uy; 2uy;
                    0uy; 3uy;
                    0uy; 4uy;
                    0uy; 5uy;
                    2uy; 0uy;
                    2uy; 1uy;
                    2uy; 2uy;
                    2uy; 3uy;
                    2uy; 4uy
                  ]

                Expect.equal
                  (t.Serialize ())
                  expected
                  "[4uy; 20uy; 00uy; 01uy...]"
            }


        ]

        testList "WriteDoResponse" [
            // the client needs to keep track
            // of the transaction, the offset and requested quantity
            // so much complexity to save a few bytes!
            test "true" {
                let t : WriteDoResponse = {
                    Offset = 0x20us
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
                    Offset = 0x20us
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
                    Offset = 0x2120us
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
            // of the transaction, the offset and requested quantity
            // so much complexity to save a few bytes!
            test "high offset and value" {
                let t : WriteRegResponse = {
                    Offset = 0x3D20us
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

        testList "WriteDosResponse" [
            // the client needs to keep track
            // of the transaction, the offset and requested quantity
            // so much complexity to save a few bytes!
            test "high offset and value" {
                let t : WriteDosResponse = {
                    Offset = 0x3D20us
                    Count = 20us
                }

                let expected =
                  [
                    15uy;
                    0x3Duy;
                    0x20uy;
                    0x00uy;
                    20uy;
                  ]

                Expect.equal
                  (t.Serialize ())
                  expected
                  "[15uy; 3Duy; 20uy; 0x00uy; 0x20uy]"
            }

        ]

        testList "WriteRegsResponse" [
            // the client needs to keep track
            // of the transaction, the offset and requested quantity
            // so much complexity to save a few bytes!
            test "high offset and value" {
                let t : WriteRegsResponse = {
                    Offset = 0x3D20us
                    Count = 20us
                }

                let expected =
                  [
                    16uy;
                    0x3Duy;
                    0x20uy;
                    0x00uy;
                    20uy;
                  ]

                Expect.equal
                  (t.Serialize ())
                  expected
                  "[16uy; 3Duy; 20uy; 0x00uy; 0x20uy]"
            }

        ]

        testList "Mbap" [
            test "wrapPdu" {
                let res : WriteRegsResponse = {
                    Offset = 0x3D20us
                    Count = 20us
                }

                let mockReqMbap  = {
                    TransactionIdentifier = 0x3234us
                    ProtocolIdentifier = 0us
                    Length = 20us
                    UnitIdentifier = 1uy

                }

                let pdu = res.Serialize ()

                let t = Mbap.wrapPdu mockReqMbap pdu

                let e = [
                    0x32uy; // trans id msB
                    0x34uy; // trans id lsB
                    00uy;   // protocol id msB (should be zero)
                    00uy;   // protocol id lsB (should be zero)
                    00uy; // length msB
                    06uy; // length lsB

            (* 1 *) 01uy; // unit id
            (* 2 *) 16uy; // function code
            (* 3 *) 0x3Duy;  // offset msB
                    0x20uy;  // offset lsB
                    0x00uy;  // count msB
            (* 6 *) 20uy;    // count lsB
                ]

                Expect.equal t e ""
            }

        ]


    ]