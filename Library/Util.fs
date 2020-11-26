module Util
// for my code, the head should always be the least significant

open System
open System.Threading
#nowarn "25"
// okay to crash if the pattern doesn't match,
// may be better to take a byte list and attempt the pattern matching inside a try-with?
// returning a result type..

let tU32 (a :: b :: c :: [d]) = BitConverter.ToUInt32([|a; b; c; d|],0)
let tI32 (a :: b :: c :: [d]) = BitConverter.ToInt32([|a; b; c; d|],0)

let tU16 (a :: [b]) = BitConverter.ToUInt16([|a; b|],0)
let tI16 (a :: [b]) = BitConverter.ToInt16([|a; b|],0)

let byteToBool (x : byte) =
  [0..7] |> List.map (fun y -> x >>> y &&& 1uy = 1uy)

let bytesToBool (x : byte list) =
  x |> List.fold (fun x y -> x @ byteToBool y ) []

let bytesToUint16 (x : byte list) =
  List.foldBack (fun x (l,r) -> x::r, l) x ([],[])
  |> fun (high,low) -> List.zip low high
  |> List.map (fun (high,low) -> [low; high] |> tU16)

// not going to test this
let BoolToByte (x : bool) =
  match x with
  | true -> 1uy
  | false -> 0uy


// the resulting list should have the lowest value
// bytes at head, i.e. [1..32] -> [1->8]@[9->16]@[17->24]@[25->32]
let BoolsToBytes (bList : bool list) : byte list =
  let byteCount = ((bList |> List.length) - 1) / 8 + 1
  [1..byteCount]
  |> List.map (
       fun x ->
         let byteIndexes = [0..7]

         let a =
           byteIndexes
           |> List.map (fun y -> (x - 1) * 8 + y, y)

         let b =
           a
           |> List.map (fun (x,y) -> (bList |> List.tryItem(x) |> function | Some x -> x | _ -> false),y)

         let c =
           b
           |> List.fold (fun a (n, i) -> (BoolToByte n) <<< i |> (+) a) 0uy

         c
    )

let U16ToBytes (x : UInt16) : byte list =
  let b1 = x |> byte
  let b2 = (x - (b1 |> uint16)) >>> 8 |> byte
  [b1; b2]

let U16sToBytes : UInt16 list -> byte list =
  List.map U16ToBytes >> List.concat

let BoolToUint16 : bool -> UInt16 =
  function | false -> 0us | true -> 255us <<< 8

let swapU16s (x : byte list) : byte list =
  let len = (x |> List.length) / 2
  [0..len-1]
  |> List.map (
       fun y ->
          let a = x |> List.item (y * 2 )
          let b = x |> List.item (y * 2 + 1)
          a,b
       )
  |> List.fold (fun a (b,c) -> a @ [c] @ [b]  ) []

module Async = 
  let CancelServer (job : Async<_>) (ctoken : CancellationToken ) = 
    let rec t () = 
      async {
        match ctoken.IsCancellationRequested with 
        | true -> return ()
        | _ -> 
          do! job
          return! t ()
      }
    t ()

  let ForeverServer job = 
    let ctoken = CancellationToken()
    CancelServer job ctoken

  let result x = async { return x }

  let map f M =
    async {
      let! m = M
      return f m
    }

  let bind f M= 
    async {
      let! m = M
      return! f m
    }