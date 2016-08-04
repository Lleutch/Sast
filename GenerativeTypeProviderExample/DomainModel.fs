module GenerativeTypeProviderExample.DomainModel

open System.Threading
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes 
open System.IO
// Scribble Type 
type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":0 , "localRole":"StringLocalRole" , "partner":"StringPartner" , "label":"StringLabel" , "payload":["StringTypes"] , "type":"EventType" , "nextState":0  } ] """>


type Buf<'T>() =
    inherit Tasks.TaskCompletionSource<'T>()

    member this.getValue() =
        this.Task.Result

let internal newTask (buf:Buf<'T>) (result:'T) (time:int) =
    Tasks.Task.Factory.StartNew(fun () -> Thread.Sleep(time*1000)
                                          buf.SetResult(result) ) |> ignore

// Agent Type + Messages Types = DU 
type Agent<'T> = MailboxProcessor<'T> 
 
type Message =
    |SendMessage of byte [] * string // (serialized message to be put in the tcp Stream , role of the partner)
    |ReceiveMessage of byte[] list * string * string list * AsyncReplyChannel<byte [] list> // (serialized message to be put in the tcp Stream , the reply channel , role of the partner)
 

// TYPE PROVIDER'S ASSEMBLY (for generative type provider) + NAMESPACE + BASETYPE 
let internal ns = "GenerativeTypeProviderExample.Provided"
let asm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
let baseType = typeof<obj>