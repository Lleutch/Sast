module ScribbleGenerativeTypeProvider.IO

open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Net.Sockets
open System.IO
open System.Text
open System
open Microsoft.FSharp.Quotations
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.Regarder
open ScribbleGenerativeTypeProvider.CommunicationAgents
open System.Threading.Tasks


type IOFailures =
    | Encoding of string*Exception
    | SerializationPayload of obj*string
    | DeserializationConvertion of (byte [])*string
    interface IFailure with
        member this.Description =
            match this with
            | Encoding (encode,exn)                     -> sprintf "IOFailures[Encoding] : Impossible to encode( %s ) \-> exception( %s )" encode (exn.Message)
            | SerializationPayload (arg,typing)         -> sprintf "IOFailures[SerializationPayload] : Cannot serialize payloads( %A ) to type( %A )" arg typing
            | DeserializationConvertion (arg,typing)    -> sprintf "IOFailures[DeserializationConvertion] : Cannot deserialze payloads( %A ) to type( %A )" arg typing


// Helpers to write and read bytes with the help of delims
let toBytes (str : string) =
    try
        let encoder = new UTF8Encoding()
        encoder.GetBytes(str)
    with
    | e -> 
        Encoding (str,e) |> createFailure

//
//let writeBytes (outputStream : Stream) (buf : byte[])  =
//    let dos = new BinaryWriter(outputStream)
//    dos.Write(buf)

// Serialization + Deserialization + DesAsync + DesChoice
// Currently only working for basic types.
// Need to find a better way to handle IO properly!!
let serLabel (label:string) (delim:string) =
    let labelBytes = label |> toBytes
    let delimBytes = delim |> toBytes
    delimBytes |> Array.append labelBytes

let getIntValues (args: Expr list) = 
    args |> List.map (fun arg -> Expr.Coerce(arg,typeof<int>) |> unbox<int []>)
    
let serPayloads (args:Expr list) (listTypes:string list) (payloadDelim:string) (endDelim:string) =
    let listPayloads =  
        args 
        |> List.mapi (fun i arg -> 
            let currentType = listTypes.[i]
            match currentType with
            |"System.String" | "System.Char"-> 
                <@  
                    let spliced = %%(Expr.Coerce(arg,typeof<obj>))
                    try
                        Type.GetType("System.Text.UTF8Encoding")
                            .GetMethod("GetBytes",[|Type.GetType(currentType)|])
                            .Invoke(new UTF8Encoding(),[|spliced|]) |> unbox<byte []> 
                    with
                    | _ -> 
                        printing "Failed to serialize 1" ""
                        SerializationPayload (spliced,currentType) |> createFailure
                            
                @>
            | _ -> 
                <@ 
                    let spliced = %%(Expr.Coerce(arg,typeof<obj>))
                    try
                        Type.GetType("System.BitConverter")
                            .GetMethod("GetBytes",[|Type.GetType(currentType)|])
                            .Invoke(null,[|spliced|] ) |> unbox<byte []> 
                    with
                    | _ -> 
                        printing "Failed to serialize 2" ""
                        SerializationPayload (spliced,currentType) |> createFailure   
                @> 
           ) 

    let numberOfPayloads = listPayloads.Length

    let listDelims = 
        [ 
            for i in 0..(numberOfPayloads-1) do
                if ( i = (numberOfPayloads-1) ) then
                    yield <@ (endDelim |> toBytes) @>
                else 
                    yield <@ (payloadDelim |> toBytes) @> ]


    listPayloads 
    |> List.fold2 (fun acc f1 f2 -> 
        <@ 
            let f1 = %f1
            let f2 = %f2
            let acc = %acc

            Array.append (Array.append acc f1) f2 
        @>
       ) <@ [||] @> <| listDelims


//let serialize (label:string) (args:Expr list) (listTypes:string list) (payloadDelim:string) (endDelim:string) (labelDelim:string) =
//    let labelSerialized = <@ serLabel label labelDelim @>
//    let payloadSerialized = serPayloads args listTypes payloadDelim endDelim 
//    <@ 
//        result{
//            let! labelSerialized = %labelSerialized 
//            let! payloadSerialized = %payloadSerialized
//            return Array.append labelSerialized payloadSerialized 
//        }
//    @>


let serialize (label:string) (args:Expr list) (listTypes:string list) (payloadDelim:string) (endDelim:string) (labelDelim:string) =
    let labelSerialized = <@ serLabel label labelDelim @> // 
                             
//    let payloadSerialized = serPayloads args listTypes payloadDelim endDelim 
    <@  
        printing "About to serialized" ""
        let payloadSerialized = %( serPayloads args listTypes payloadDelim endDelim )
        let labelSerialized  = %(labelSerialized) 
        printing "Serialization done" ""
        Array.append labelSerialized payloadSerialized 
    @>        

let convert (arrayList:byte[] list) (elemTypelist:string list) =
    let rec aux (arrList:byte[] list) (elemList:string list) (acc:obj list) =
        match arrList with
        |[] -> List.rev acc
        |hd::tl ->  
            let sub = elemList.Head.Split('.')
            let typing = sub.[sub.Length-1]
            try
                let mymethod = Type.GetType("System.BitConverter")
                                    .GetMethod("To"+typing,[|typeof<byte []>;typeof<int>|])
                let invoke = mymethod.Invoke(null,[|box hd;box 0|])
                aux tl (elemList.Tail) (invoke::acc)
            with
            | e -> 
                DeserializationConvertion (hd,typing) |> createFailure  
                        
    aux arrayList elemTypelist []
   
let deserialize (args: Expr list)  (listTypes:string list) (messages: _ list) (role:string) =
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]

    <@ 
        let result = Regarder.receiveMessage "agent" messages role [listTypes]
        printfn " deserialize Normal : %A || Role : %A || listTypes : %A" messages role listTypes
        printing " received Bytes: " result
            
        let received = convert (result.Tail) listTypes 
        let received = List.toSeq received
        Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
    @>
                 

let deserializeAsync (args: Expr list)  (listTypes:string list) (messages: _ list) (role:string) =  
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]              
    <@ 
        let work = 
            async{            
                let! res = Regarder.receiveMessageAsync "agent" messages role listTypes 
                let res =
                    let received = (res.Tail |> convert <| listTypes )
                    let received = received |> List.toSeq          
                    Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
                return res
            }
        Async.Start(work)
     @>


let deserializeChoice (args: Expr list) (listTypes:string list) =
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]
    <@ 
        let result = Regarder.receiveChoice "agent" 
        let received = (result |> convert <| listTypes ) 
        let received = received |> List.toSeq          
        //let received = result |> List.toSeq
        let test = (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
        let completed = test |> Array.map(fun t -> t.GetTask())
        printing "Receive a choice" (received,test,completed)
        Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
    @>