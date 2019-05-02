module Logging 
open LoggingTypes
open System
open System.Text
open Hopac 
open Hopac.Infixes

module ConsoleEndpoint = 
  let build () : Job<Message -> Job<unit>> = 
    let mailbox : Mailbox<byte []> = Mailbox()
    
    let s = 
      job {
        let! a = mailbox |> Mailbox.take
        let s = Encoding.ASCII.GetString(a)
        printfn "%s" s
      } |> Job.foreverServer

    // no colorization, logs everything to console
    let Log () = 
      job {
        return fun (msg : Message) -> 
        
          let dt = msg.DateTime |> DateTime'.ToISO
          let logLevel = msg.LogLevel.ToString()
          let message = 
            let fields = msg.Fields |> Map.toList
            fields 
            |> List.fold (
               fun (m : String) (k,o) -> 
                 try // things can go wrong here, shouldn't punish the person using this! 
                   let k' : string [] = [| sprintf "{%s}" k |]
                   let [head; tail] = m.Split(k', StringSplitOptions.None) |> Seq.toList
                   let os = 
                      let ts = sprintf "%A" o
                      match ts.Length with
                      | x when x <= 20 -> ts
                      | x -> ts.Substring(0, 20) |> sprintf "%s..."
                   let s : string = sprintf "%s%s%s" head os tail
                   s
                 with | _ -> m
               ) msg.Message
              
          let tags = 
            match msg.Tags with 
            | [] -> ""
            | _ -> 
              let tags = msg.Tags |> List.sort
              let maxLength = tags |> List.map (fun x -> x.Length) |> List.max
              let format = sprintf "{0,%d}" maxLength
              let tags = tags |> List.map (fun x -> String.Format(format, x))
              msg.Tags |> List.reduce (fun a n -> sprintf "%A, %A"a n)
          
          job {
            let t : string = sprintf "%s | %s | %s | %s " dt logLevel message tags 
            let a = Encoding.ASCII.GetBytes(t)
            do! a |> Mailbox.send mailbox
          } |> Job.queueIgnore
      }
    
    s >>= Log