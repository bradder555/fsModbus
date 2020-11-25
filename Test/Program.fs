// Learn more about F# at http://fsharp.org

open System
open Tests

[<EntryPoint>]
let main argv =
  runTests |> Async.RunSynchronously
  0
