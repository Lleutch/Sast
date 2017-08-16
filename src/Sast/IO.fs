module GenerativeTypeProviderExample.IO

open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Net.Sockets
open System.IO
open System.Text
open System
open Microsoft.FSharp.Quotations
open GenerativeTypeProviderExample.DomainModel
open GenerativeTypeProviderExample.Regarder
open GenerativeTypeProviderExample.CommunicationAgents
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
    result{
        try
            let encoder = new UTF8Encoding()
            return encoder.GetBytes(str)
        with
        | e -> 
            return! Encoding (str,e) |> createFailure
    }
//
//let writeBytes (outputStream : Stream) (buf : byte[])  =
//    let dos = new BinaryWriter(outputStream)
//    dos.Write(buf)

// Serialization + Deserialization + DesAsync + DesChoice
// Currently only working for basic types.
// Need to find a better way to handle IO properly!!
let serLabel (label:string) (delim:string) =
        result{
            let! labelBytes = label |> toBytes
            let! delimBytes = delim |> toBytes
            return delimBytes |> Array.append labelBytes
        }

let serPayloads (args:Expr list) (listTypes:string list) (payloadDelim:string) (endDelim:string) =
    let listPayloads =  
        args 
        |> List.mapi (fun i arg -> 
            let currentType = listTypes.[i]
            match currentType with
            |"System.String" | "System.Char"-> 
                <@  
                    let spliced = %%(Expr.Coerce(arg,typeof<obj>))
                    result{
                        try
                            return 
                                Type.GetType("System.Text.UTF8Encoding")
                                    .GetMethod("GetBytes",[|Type.GetType(currentType)|])
                                    .Invoke(new UTF8Encoding(),[|spliced|]) |> unbox<byte []> 
                        with
                        | _ -> 
                            return! SerializationPayload (spliced,currentType) |> createFailure
                            
                    }
                @>
            | _ -> 
                <@ 
                    let spliced = %%(Expr.Coerce(arg,typeof<obj>))
                    result{
                        try
                            return 
                                Type.GetType("System.BitConverter")
                                    .GetMethod("GetBytes",[|Type.GetType(currentType)|])
                                    .Invoke(null,[|spliced|] ) |> unbox<byte []> 
                        with
                        | _ -> 
                            return! SerializationPayload (spliced,currentType) |> createFailure   
                    }
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
            result{
                let! f1 = %f1
                let! f2 = %f2
                let! acc = %acc

                return Array.append (Array.append acc f1) f2 
            }
        @>
       ) <@ result{ return [||]} @> <| listDelims


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
        result{ 
            let! payloadSerialized = %( serPayloads args listTypes payloadDelim endDelim )
            let! labelSerialized  = %(labelSerialized) 
            return Array.append labelSerialized payloadSerialized 
        } 
    @>        

let convert (arrayList:byte[] list) (elemTypelist:string list) =
    let rec aux (arrList:byte[] list) (elemList:string list) (acc:obj list) =
        result{
            match arrList with
                |[] -> return List.rev acc
                |hd::tl ->  
                    let sub = elemList.Head.Split('.')
                    let typing = sub.[sub.Length-1]
                    try
                        let mymethod = Type.GetType("System.BitConverter")
                                           .GetMethod("To"+typing,[|typeof<byte []>;typeof<int>|])
                        let invoke = mymethod.Invoke(null,[|box hd;box 0|])
                        return! aux tl (elemList.Tail) (invoke::acc)
                    with
                    | e -> 
                        return! DeserializationConvertion (hd,typing) |> createFailure  
                        
        }
    aux arrayList elemTypelist []
   
let deserialize (args: Expr list)  (listTypes:string list) (messages: _ list) (role:string) =
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]

    <@ 
        result{
            let result = Regarder.receiveMessage "agent" messages role listTypes 
            printfn " deserialize Normal : %A || Role : %A || listTypes : %A" messages role listTypes
            printing " received Bytes: " result
            
            let! received = convert (result.Tail) listTypes 
            let received = List.toSeq received
            Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
        }
    @>
                 

let deserializeAsync (args: Expr list)  (listTypes:string list) (messages: _ list) (role:string) =  
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]              
    <@ 
        let work = 
            async{            
                let! res = Regarder.receiveMessageAsync "agent" messages role listTypes 
                let res =
                    result{
                        let! received = (res.Tail |> convert <| listTypes )
                        let received = received |> List.toSeq          
                        Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
                    }
                return res
            }
        // This modification breaks the "asynchronism that we achieved before"
        let res = Async.RunSynchronously(work)
        res
     @>


let deserializeChoice (args: Expr list) (listTypes:string list) =
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]
    <@ 
        result{
            let result = Regarder.receiveChoice "agent" 
            let! received = (result |> convert <| listTypes ) 
            let received = received |> List.toSeq          
            //let received = result |> List.toSeq
            Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
        }
    @>