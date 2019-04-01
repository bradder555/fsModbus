module Test.Util

open Hopac
open Expecto
open ModbusTypes
open Util 

let tests = 
    testList "Util" [
        testList "BoolsToBytes" [
          test "one bit true" {
            let bit = [true]
            let r = BoolsToBytes bit
            Expect.equal r [1uy] "[true] should equal [1uy]"
          }
          test "one bit false" {
            let bit = [false]
            let r = BoolsToBytes bit
            Expect.equal r [0uy] "[false] should equal [0uy]"
          }
          test "4 bits, 1 true at 0" {
            let bits = [true; false; false; false]
            let r = BoolsToBytes bits
            Expect.equal r [1uy] "[true; false; false; false] should equal [1uy]"
          }
          test "4 bits, 1 true at 2" {
            let bits = [false; false; true; false]
            let r = BoolsToBytes bits
            Expect.equal r [4uy] "[false; false; true; false] should equal [4uy]"
          }
          test "8 bits, 1 true at 2" {
            let bits = [0..7] |> List.map (fun x -> x = 2)
            let r = BoolsToBytes bits
            Expect.equal r [4uy] "[0..7] |> List.map (fun x -> x = 2) should equal [4uy]"
          }
          test "8 bits, 1 true at 1 & 5" {
            let bits = [0..7] |> List.map (fun x -> x = 1 || x = 5)
            let r = BoolsToBytes bits
            Expect.equal r [2uy + 32uy] "[0..7] |> List.map (fun x -> x = 1 || x = 5) should equal [34uy]"
          }
          test "12 bits, 1 true at 10" {
            let bits = [0..12] |> List.map (fun x -> x = 10)
            let r = BoolsToBytes bits
            Expect.equal r [0uy; 4uy] "[0..12] |> List.map (fun x -> x = 10) should equal [0uy; 4uy]"
          }
          test "29 bits, 1 true at 10, 21 & 29" {
            // ........ ..1..... .....1.. .....1 
            let bits = [0..29] |> List.map (fun x -> x = 10 || x = 21 || x = 29)
            let r = BoolsToBytes bits
            Expect.equal r [0uy; 4uy; 32uy; 32uy] "should equal [0uy; 4uy; 32uy; 32uy]"
          }
          test "32 bits, 1 true at 10, 21 & 31" {
            // ........ ..1..... .....1.. .......1
            let bits = [0..31] |> List.map (fun x -> x = 10 || x = 21 || x = 31)
            let r = BoolsToBytes bits
            Expect.equal r [0uy; 4uy; 32uy; 128uy] "should equal [0uy; 4uy; 32uy; 128uy]"
          }
          test "32 bits, last byte 1s" {
            // ........ ........ ........ 11111111
            let bits = [0..31] |> List.map (fun x -> x > 23)
            let r = BoolsToBytes bits
            Expect.equal r [0uy; 0uy; 0uy; 255uy] "should equal [0uy; 0uy; 0uy; 255uy]"
          }        
        ];
        testList "tU32" [ 
          test "one byte 1uy" {
            let r = [1uy; 0uy; 0uy; 0uy] |> tU32
            Expect.equal r 1u "should equal 1u"
          }
          test "second byte 1uy" {
            let r = [0uy; 1uy; 0uy; 0uy] |> tU32
            Expect.equal r 256u "should equal 512u"
          }
          test "3uy; 3uy" {
            // 11...... 11......
            // 1 2 4 8 16 32 64 128   256 512 1024 ...
            let r = [3uy; 3uy; 0uy; 0uy] |> tU32
            let e = 3u + 256u + 512u 
            Expect.equal r e "should equal 771u"
          }
        ];
        testList "tI32" [ 
          test "one byte 1uy" {
            let r = [1uy; 0uy; 0uy; 0uy] |> tI32
            Expect.equal r 1 "should equal 1u"
          }
          test "second byte 1uy" {
            let r = [0uy; 1uy; 0uy; 0uy] |> tI32
            Expect.equal r 256 "should equal 512u"
          }
          test "3uy; 3uy" {
            // 11...... 11......
            // 1 2 4 8 16 32 64 128   256 512 1024 ...
            let r = [3uy; 3uy; 0uy; 0uy] |> tI32
            let e = 771
            Expect.equal r e "should equal 771"
          }
        ];
        testList "tU16" [ 
          test "one byte 1uy" {
            let r = [1uy; 0uy; ] |> tU16
            Expect.equal r 1us "should equal 1u"
          }
          test "second byte 1uy" {
            let r = [0uy; 1uy; ] |> tU16
            Expect.equal r 256us "should equal 512u"
          }
          test "3uy; 3uy" {
            // 11...... 11......
            // 1 2 4 8 16 32 64 128   256 512 1024 ...
            let r = [3uy; 3uy; ] |> tU16
            let e = 771us
            Expect.equal r e "should equal 771"
          }
        ];
        testList "tI16" [ 
          test "one byte 1uy" {
            let r = [1uy; 0uy; ] |> tI16
            Expect.equal r 1s "should equal 1s"
          }
          test "second byte 1uy" {
            let r = [0uy; 1uy; ] |> tI16
            Expect.equal r 256s "should equal 512s"
          }
          test "3uy; 3uy" {
            // 11...... 11......
            // 1 2 4 8 16 32 64 128   256 512 1024 ...
            let r = [3uy; 3uy; ] |> tI16
            let e = 771s
            Expect.equal r e "should equal 771s"
          }
        ];
        testList "byteToBool" [ 
          test "0uy" {
            let r = 0uy |> byteToBool
            let e = [0..7] |> List.map(fun _ -> false)
            Expect.equal r e "should equal all falses"
          }
          test "1uy" {
            let r = 1uy |> byteToBool
            let e = [0..7] |> List.map(fun x -> x = 0)
            Expect.equal r e "the head should = true"
          }
          test "129uy" {
            let r = 129uy |> byteToBool
            let e = [0..7] |> List.map(fun x -> x = 0 || x = 7)
            Expect.equal r e "the head and last should be true"
          }        
        ]
        testList "bytesToBool" [ 
          test "[0uy]" {
            let r = [0uy] |> bytesToBool
            let e = [0..7] |> List.map(fun _ -> false)
            Expect.equal r e "should equal all falses"
          }
          test "[1uy]" {
            let r = [1uy] |> bytesToBool
            let e = [0..7] |> List.map(fun x -> x = 0)
            Expect.equal r e "the head should = true"
          }
          test "[129uy]" {
            let r = [129uy] |> bytesToBool
            let e = [0..7] |> List.map(fun x -> x = 0 || x = 7)
            Expect.equal r e "the head and last should be true"
          }
          test "[0uy;0uy]" {
            let r = [0uy;0uy] |> bytesToBool
            let e = [0..15] |> List.map(fun _ -> false)
            Expect.equal r e "should equal all falses"
          }
          test "[1uy;0uy]" {
            let r = [1uy;0uy] |> bytesToBool
            let e = [0..15] |> List.map(fun x -> x = 0)
            Expect.equal r e "the head should = true"
          }
          test "[0uy;1uy]" {
            let r = [0uy;1uy] |> bytesToBool
            let e = [0..15] |> List.map(fun x -> x = 8)
            Expect.equal r e "the head should = true"
          }        
          test "[1uy;129uy]" {
            let r = [1uy;129uy] |> bytesToBool
            let e = [0..15] |> List.map(fun x -> x = 0 || x = 8 || x = 15)
            Expect.equal r e "the head and last should be true"
          }
          test "[1uy;129uy;0uy]" {
            let r = [1uy;129uy;0uy] |> bytesToBool
            let e = [0..23] |> List.map(fun x -> x = 0 || x = 8 || x = 15)
            Expect.equal r e "the head and last should be true"
          }                  
        ]
        testList "bytesToUint16" [ 
          test "[1uy; 0uy]" {
            let r = [1uy; 0uy] |> bytesToUint16
            Expect.equal r [1us] "should be 1us"
          }
          test "[1uy; 0uy; 2uy; 1uy]" {
            let r = [1uy; 0uy; 2uy; 1uy;] |> bytesToUint16
            Expect.equal r [1us; 258us] "should be [1us; 259us]"
          }
          test "[1uy; 0uy; 2uy; 1uy; 0uy; 1uy]" {
            let r = [1uy; 0uy; 2uy; 1uy; 0uy; 1uy;] |> bytesToUint16
            Expect.equal r [1us; 258us; 256us] "should be [1us; 259us]"
          }        
        ]
        testList "U16ToBytes"[
          test "1us" {
            let r = 1us |> U16ToBytes
            Expect.equal r [1uy; 0uy] "1us should yield [1uy; 0uy]"
          }
          test "258us" {
            let r = 258us |> U16ToBytes
            Expect.equal r [2uy; 1uy] "258us should yield [2uy; 1uy]"
          }        
        ]
        testList "U16sToBytes"[
          test "[1us]" {
            let r = [1us] |> U16sToBytes
            Expect.equal r [1uy; 0uy] "1us should yield [1uy; 0uy]"
          }
          test "[258us]" {
            let r = [258us] |> U16sToBytes
            Expect.equal r [2uy; 1uy] "258us should yield [2uy; 1uy]"
          }        
          test "[1us; 2us]" {
            let r = [1us; 2us] |> U16sToBytes
            Expect.equal r [1uy; 0uy; 2uy; 0uy] "[1us; 2us] should yield [1uy; 0uy; 2uy; 0uy]"
          }
          test "[264us; 513us]" {
            let r = [264us; 513us] |> U16sToBytes
            Expect.equal r [8uy; 1uy; 1uy; 2uy] "[264us; 513us] should yield [8uy; 1uy; 1uy; 2uy]"
          }           
        ]      
        testList "swapU16s"[
          test "[1uy; 0uy]" {
            let r = [1uy; 0uy] |> swapU16s
            Expect.equal r [0uy; 1uy] "should yield [0uy; 1uy]"
          }
          test "[0uy; 1uy]" {
            let r = [0uy; 1uy] |> swapU16s
            Expect.equal r [1uy; 0uy] "should yield [1uy; 0uy]"
          }        
          test "[1uy; 2uy; 3uy; 4uy]" {
            let r = [1uy; 2uy; 3uy; 4uy] |> swapU16s
            Expect.equal r [2uy; 1uy; 4uy; 3uy] "should yield [2uy; 1uy; 4uy; 3uy]"
          }
          test "[1uy; 2uy; 3uy; 4uy; 5uy; 6uy]" {
            let r = [1uy; 2uy; 3uy; 4uy; 5uy; 6uy] |> swapU16s
            Expect.equal r [2uy; 1uy; 4uy; 3uy; 6uy; 5uy] "should yield [2uy; 1uy; 4uy; 3uy; 6uy; 5uy]"
          }           
        ]            
    ]