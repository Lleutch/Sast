module GenerativeTypeProviderExample.DomainModel

open System.Threading
open FSharp.Configuration

// Scribble Type 
type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":0 , "localRole":"StringLocalRole" , "partner":"StringPartner" , "label":"StringLabel" , "payload":["StringTypes"] , "type":"EventType" , "nextState":0  } ] """>

type ScribbleAPI = FSharp.Data.JsonProvider<""" { "code":"Code", "proto":"global protocol", "role":"local role" } """>

type MappingDelimiters = FSharp.Data.JsonProvider<""" [ {"label" : "string", "delims": {"delim1": ["delim1"] , "delim2": ["delim2"] , "delim3": ["delim3"] } } ] """>

// workaround for duplication of convertion between type aliases and real type.
type DotNetTypesMapping = FSharp.Data.JsonProvider<""" [ {"alias" : "aliasType", "type": "RealType"} ] """>

type ISetResult =
    abstract member SetValue : obj -> unit

type Buf<'T>() =
    inherit Tasks.TaskCompletionSource<'T>()
    member this.getValue() =
        this.Task.Result
    interface ISetResult with
        member this.SetValue(res) =
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
//let baseType = typeof<obj>

let mutable mappingDelimitateur = Map.empty<string,string list * string list * string list>

let modifyMap delim = 
    mappingDelimitateur <- delim

let getDelims label =
    match (mappingDelimitateur.TryFind label) with
        | None -> sprintf "the following label %s has not been defined in the list of delimiters" label |> failwith
        | Some delims -> delims

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


// Result Monad + End type 
type End internal () = class end

type IFailure =
    abstract member Description: string

type Test =
    | Test
    interface IFailure with
        member this.Description =
            match this with
            | Test -> sprintf "Test"

// Change string to a IFailure interface that needs to be implemented
// by groups of errors.
[<NoComparison>]
[<NoEquality>]
type Result<'a> =
    | Success of 'a
    | Failure of IFailure
 
[<NoComparison>]
[<NoEquality>]
type ResultWrapped<'a> = 
    |ResultWrapped of (unit -> Result<'a>)
        static member Cast<'a>(ResultWrapped(untRW) : ResultWrapped<obj>) : ResultWrapped<'a> =        
            (fun () ->                        
                let untR = untRW ()            
                match untR with            
                | Success o ->                
                    match o with                
                    | :? 'a as a -> Success a                
                    | _ -> Failure Test // ... your failure handler for wrong type conversion            
                | Failure f -> 
                    Failure f            
            ) |> ResultWrapped

type ResultBuilder() =
    member __.Bind(ResultWrapped m, f) =
        match m() with
        |Success elem -> f elem
        |Failure s -> Failure s

    member __.Return x = Success x
    member __.ReturnFrom(ResultWrapped m) = m()
    member __.Zero () = Success ()

    member __.Combine (a:Result<'a>,b)= 
        let runnedB = b()
        match a,runnedB with 
        |Success a1 , Success b1      -> Success b1
        |Success a1 , Failure b1    -> Failure b1
        |Failure a1 , Success b1    -> Failure a1
        |Failure a1 , Failure b1  -> Failure a1

    member __.Delay(f:unit -> Result<'a>) = f

    member __.Run(delayed) = ResultWrapped delayed

    member this.TryWith(body:unit -> Result<'a> ,handler) =
        try
//            let (ResultWrapped body) = body
            this.ReturnFrom(ResultWrapped body)
        with
        e ->
            handler e        

            
let result = ResultBuilder()


let createFailure (failure:#IFailure) = 
    fun() -> Failure failure
    |> ResultWrapped

let internal run (oct:ResultWrapped<'a>) = 
    let (ResultWrapped oct) = oct
    let res = oct() 
    res

let internal makeRun (oct:ResultWrapped<'a>) = 
    let (ResultWrapped oct) = oct
    let res = oct() 
    <@ res @>

let inline runResult (handler: Result<End> -> unit) (oct:ResultWrapped<End>) = 
    let (ResultWrapped oct) = oct
    let res = oct() 
    handler res



let test = 
    result{
        let value = failwith "error"
        return 5
    }

let test2 =
    result {
        try
            let! value = test
            return 6 + value
        with 
        e ->
             return! Test |> createFailure
    }

let (ResultWrapped oct) = test2
let res = oct() 

let baseType = typeof<obj>



module ess =
    open FSharp.Quotations
    open FSharp.Quotations.Evaluator


    type Teste() = class end

    let q1 = Expr.NewObject((typeof<Teste>.GetConstructors().[0]),[])

    let q2 = 
        <@@
            result{
                let t = %%Expr.Coerce(q1,typeof<obj>)//,typeof<Teste>)
                return t
            }
        @@>
        |> FSharp.Quotations.Evaluator.QuotationEvaluator.CompileUntyped


type record = { aFun : string -> bool }

    

module QuotationHelpers = 

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection

    let rec coerceValues fieldTypeLookup fields = 
        Array.mapi (fun i v ->
                let expr = 
                    if v = null then simpleTypeExpr v
                    elif FSharpType.IsUnion (v.GetType()) then unionExpr v |> snd
                    elif FSharpType.IsRecord (v.GetType()) then recordExpr v |> snd
                    else simpleTypeExpr v
                Expr.Coerce(expr, fieldTypeLookup i)
        ) fields |> List.ofArray
    
    and simpleTypeExpr instance = Expr.Value(instance)

    and unionExpr instance = 
        let caseInfo, fields = FSharpValue.GetUnionFields(instance, instance.GetType())    
        let fieldInfo = caseInfo.GetFields()
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        caseInfo.DeclaringType, Expr.NewUnionCase(caseInfo, coerceValues fieldTypeLookup fields)

    and recordExpr instance = 
        let tpy = instance.GetType()
        let fields = FSharpValue.GetRecordFields(instance)
        let fieldInfo = FSharpType.GetRecordFields(tpy)
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        tpy, Expr.NewRecord(instance.GetType(), coerceValues fieldTypeLookup fields)

    and arrayExpr (instance : 'a array) =
        let typ = typeof<'a>
        let arrayType = instance.GetType()
        let exprs = coerceValues (fun _ -> typ) (instance |> Array.map box)
        arrayType, Expr.NewArray(typ, exprs)

    let createLetExpr varType instance body args = 
        let var = Var("instance", varType)  
        Expr.Let(var, instance, body args (Expr.Var(var)))

    let quoteUnion instance = unionExpr instance ||> createLetExpr
    let quoteRecord instance = recordExpr instance ||> createLetExpr
    let quoteArray instance = arrayExpr instance ||> createLetExpr



