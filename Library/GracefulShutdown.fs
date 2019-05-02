module GracefulShutdown

open Hopac
open Hopac.Infixes
open System

type GracefulShutdown = 
  {
    Alt : Alt<unit>
    Finished : unit -> unit
  }
with static member Build () : GracefulShutdown = 
      let finished = IVar()
      let alt' = 
        fun nack -> job {
            let c = IVar()
            do!
              job {
                Console.ReadLine() |> ignore
                printfn "enter pressed"
                do! IVar.tryFill c ()
              } |> Job.queueIgnore

            do!
              job {
                AppDomain.CurrentDomain.ProcessExit.Add (
                  fun e ->
                    () |> IVar.tryFill c |> run
                    IVar.read finished |> run // block here to allow the application to close
                    ()
                )

              }
            return IVar.read c ^-> fun _ -> printfn "gracefully shutting down"
        } |> Alt.withNackJob

      let finished () = IVar.tryFill finished () |> start
      {
        Alt = alt'
        Finished = finished
      }