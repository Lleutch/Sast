module GenerativeTypeProviderExample.DomainModel


// Scribble Type 
type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>

// Agent Type + Messages Types = DU 
type Agent<'T> = MailboxProcessor<'T> 
 
type Message =
    |SendMessage of byte [] * string // (serialized message to be put in the tcp Stream , role of the partner)
    |ReceiveMessage of byte [] * AsyncReplyChannel<byte []> * string // (serialized message to be put in the tcp Stream , the reply channel , role of the partner)
 

// TYPE PROVIDER'S ASSEMBLY (for generative type provider) + NAMESPACE + BASETYPE 
let internal ns = "GenerativeTypeProviderExample.Provided"
//let asm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
let baseType = typeof<obj>