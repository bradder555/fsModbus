// Learn more about F# at http://fsharp.org

open Hopac
open Hopac.Infixes
open System
open Tests
open GracefulShutdown

[<EntryPoint>]
let main argv =
  let gracefulShutdown = GracefulShutdown.Build()
  let testJob = (runTests >>- (fun _ -> ())) |> Promise.queue |> Alt.prepare
  (gracefulShutdown.Alt <|> testJob) |> Hopac.run

  // horrible global to tell processexit that the app has finished gracefully
  gracefulShutdown.Finished ()
  0
