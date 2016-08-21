module GenerativeTypeProviderExample.DomainModel

open System.Threading
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes 
open System.IO
open FSharp.Configuration

// Scribble Type 
type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":0 , "localRole":"StringLocalRole" , "partner":"StringPartner" , "label":"StringLabel" , "payload":["StringTypes"] , "type":"EventType" , "nextState":0  } ] """>

type ScribbleAPI = FSharp.Data.JsonProvider<""" { "code":"Code", "proto":"global protocol", "role":"local role" } """>


type ISetResult =
    abstract member SetValue : obj -> unit

type Buf<'T>() =
    inherit Tasks.TaskCompletionSource<'T>()
    member this.getValue() =
        this.Task.Result
    interface ISetResult with
        member this.SetValue(res) =
            printfn "HERE IS THE TYPE OF THE RES: %s" (res.GetType().FullName)
            printfn "AND NOWWWWWWW : %s" (this.GetType().FullName)
            this.SetResult(unbox<'T> res)

// Agent Type + Messages Types = DU 
type Agent<'T> = MailboxProcessor<'T> 
 
type Message =
    |SendMessage of byte [] * string // (serialized message to be put in the tcp Stream , role of the partner)
    |ReceiveMessage of byte[] list * string * string list * AsyncReplyChannel<byte [] list> // (serialized message to be put in the tcp Stream , the reply channel , role of the partner)
    |ReceiveMessageAsync of byte[] list * string * string list * AsyncReplyChannel<byte [] list> // (serialized message to be put in the tcp Stream , the reply channel , role of the partner)


// TYPE PROVIDER'S ASSEMBLY (for generative type provider) + NAMESPACE + BASETYPE 
let internal ns = "GenerativeTypeProviderExample.Provided"
//let asm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
let baseType = typeof<obj>



// Configuration File YAML TP
[<Literal>]
let metaYaml = "Partners:
    - Name: You
      IP: 127.0.0.1
      Port: 5001

LocalRole:
  Name: Me
  IP: 127.0.0.1
  Port: 5000 "

type ConfigFile = YamlConfig<YamlText=metaYaml>
let config = ConfigFile()