module ScribbleGenerativeTypeProvider.DomainModel

open System.Threading
open FSharp.Configuration
open Common
open Common.CommonFSM
open ProviderImplementation.ProvidedTypes
open Common.CFSM
open Microsoft.FSharp.Quotations
open System


type ScribbleAPI = FSharp.Data.JsonProvider<""" { "code":"Code", "proto":"global protocol", "role":"local role" } """>

type MappingDelimiters = FSharp.Data.JsonProvider<""" [ {"label" : "string", "delims": {"delim1": ["delim1"] , "delim2": ["delim2"] , "delim3": ["delim3"] } } ] """>

// workaround for duplication of convertion between type aliases and real type.
type DotNetTypesMapping = FSharp.Data.JsonProvider<""" [ {"alias" : "aliasType", "type": "RealType"} ] """>

type ISetResult =
    abstract member SetValue : obj -> unit
    abstract member GetTask : unit -> bool

type Buf<'T>() =
    inherit Tasks.TaskCompletionSource<'T>()
    member this.getValue() =
        this.Task.Result
    interface ISetResult with
        member this.SetValue(res) =
            this.SetResult(unbox<'T> res)
        member this.GetTask() = 
            this.Task.IsCompleted


// Agent Type + Messages Types = DU 
type Agent<'T> = MailboxProcessor<'T> 
 
type Message =
    |SendMessage of byte [] * string // (serialized message to be put in the tcp Stream , role of the partner)
    |ReceiveMessage of (byte[] * string list) list * string * AsyncReplyChannel<byte [] list> // (serialized message to be put in the tcp Stream , the reply channel , role of the partner)
    |ReceiveMessageAsync of byte[] list * string * string list * AsyncReplyChannel<byte [] list> // (serialized message to be put in the tcp Stream , the reply channel , role of the partner)
    

// TYPE PROVIDER'S ASSEMBLY (for generative type provider) + NAMESPACE + BASETYPE 
let internal ns = "ScribbleGenerativeTypeProvider.Provided"
//let asm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
let baseType = typeof<obj>

let mutable mappingDelimitateur = Map.empty<string,string list * string list * string list>

let modifyMap delim = 
    mappingDelimitateur <- delim

let getDelims label =
    match (mappingDelimitateur.TryFind label) with
        | None -> sprintf "the following label %s has not been defined in the list of delimiters" label |> failwith
        | Some delims -> delims

// Configuration File YAML TP
[<Literal>]
let metaConfig = "
Partners:
    - Name: You
      IP: 127.0.0.1
      Port: 5001

LocalRole:
  Name: Me
  IP: 127.0.0.1
  Port: 5000 

Delimiters:
    LabelDelimiter      : delimiter
    EndDelimiter        : delimiter
    PayloadDelimiter    : delimiter

"
type ConfigurationFile  = YamlConfig<YamlText = metaConfig, InferTypesFromStrings=true>
let configurationFile   = ConfigurationFile()


// Result Monad + End type 
type End internal () = class end

type IFailure =
    abstract member Description: string


let createFailure (failure:#IFailure) = failwith failure.Description


let printing message data =
    let doPrinting = true
    if doPrinting then
        printfn "%s %A" message data


(*** ***************************************************************************************************** ***)
(***                              Wrapper types for ProvidedTypes Runtime                                  ***)
(*** there are also wrapper for method binding between all the providedTypes in the ProvidedTypes universe ***)
(*** ***************************************************************************************************** ***)
// TODO : Probably change the approach for serialization + deserialization
type Delimiters =
    {
        labelDelimiter      : string
        payloadDelimiter    : string
        endDelimiter        : string
    }

(*** ***************************************************************************************************** ***)
(***                                    ProvidedTypes Compile-time                                         ***)
(***          Wrapper types for going CFSM -> ProvidedTypes universe : For States + Labels + Roles         ***)
(*** there are also wrapper for method binding between all the providedTypes in the ProvidedTypes universe ***)
(*** ***************************************************************************************************** ***)
type ProvidedPartner   = ProvidedPartner of ProvidedTypeDefinition
type GeneratedPartners  = GeneratedPartners of Map<Partner,ProvidedPartner>

type ProvidedLabel     = ProvidedLabel of ProvidedTypeDefinition
type GeneratedLabels   = GeneratedLabels of Map<Label,ProvidedLabel>
type InterfaceType     = InterfaceType of Type 

type Choice =
    {   interfaceType   : InterfaceType
        branching       : ProvidedTypeDefinition
        branches        : GeneratedLabels   }

type StateTypes =
    | ChoiceType of Choice
    | NotChoiceType of ProvidedTypeDefinition
    | EndType of ProvidedTypeDefinition

type GeneratedStates    = GeneratedStates of Map<StateId,StateTypes>

type ChannelID = 
    | ChannelState of StateId
    | ChannelLabel of Label

type MethodLinkingNoBranch =
    {
        transition          : Transition 
        generatedStates     : GeneratedStates
        stateID             : StateId
        generatedPartners   : GeneratedPartners
        invokeCode          : Expr -> (Expr list -> Expr)
        methodName          : string
    }


type MethodLinkingBranch =
    {
        transitions         : Transitions 
        generatedStates     : GeneratedStates
        stateID             : StateId
        generatedPartners   : GeneratedPartners
        invokeCode          : (Expr list -> Expr)
    }

type MethodLinkingEnd<'a> =
    {
        generatedStates     : GeneratedStates
        stateID             : StateId
    }


