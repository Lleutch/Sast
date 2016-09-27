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


// Helpers to write and read bytes with the help of delims

let toBytes (str : string) =
    let encoder = new UTF8Encoding()
    encoder.GetBytes(str)

let writeBytes (outputStream : Stream) (buf : byte[])  =
    let dos = new BinaryWriter(outputStream)
    dos.Write(buf)

let readLabel (inputStream : Stream) (labelDelim : string) = 
    let dis = new BinaryReader(inputStream)
    let rec readingBytes accLabel =
        let readByte = dis.ReadByte()
        match readByte with
            | labelDelim -> let decoder = new UTF8Encoding()
                            accLabel |> decoder.GetString
            | _ -> readingBytes (Array.append accLabel [|readByte|])
    in readingBytes [||]





// Serialization + Deserialization + DesAsync + DesChoice

let serLabel (label:string) (delim:string) =
    let labelBytes = label |> toBytes
    let delimBytes = delim |> toBytes
    delimBytes |> Array.append labelBytes

let serPayloads (args:Expr list) (listTypes:string list) (payloadDelim:string) (endDelim:string) =
    let listPayloads =  args |> List.mapi (fun i arg -> 
                                    let currentType = listTypes.[i]
                                    match currentType with
                                        |"System.String" | "System.Char"-> 
                                            <@@ Type.GetType("System.Text.UTF8Encoding")
                                                    .GetMethod("GetBytes",[|Type.GetType(currentType)|])
                                                    .Invoke(new UTF8Encoding(),[|%%(Expr.Coerce(arg,typeof<obj>))|]) |> unbox<byte []> @@>
                                        | _ -> 
                                        <@@ Type.GetType("System.BitConverter")
                                                .GetMethod("GetBytes",[|Type.GetType(currentType)|])
                                                .Invoke(null,[|%%(Expr.Coerce(arg,typeof<obj>))|] ) |> unbox<byte []> @@> ) 

    let numberOfPayloads = listPayloads.Length

    let listDelims = [ for i in 0..(numberOfPayloads-1) do
                           if ( i = (numberOfPayloads-1) ) then
                               yield <@@ (endDelim |> toBytes) @@>
                           else 
                               yield <@@ (payloadDelim |> toBytes) @@> ]

    listPayloads |> List.fold2 (fun acc f1 f2 -> <@@ Array.append (Array.append (%%acc) (%%f1:byte [])) (%%f2:byte [])  @@>)  <@@ [||]:byte [] @@> <| listDelims


let serialize (label:string) (args:Expr list) (listTypes:string list) (payloadDelim:string) (endDelim:string) (labelDelim:string) =
    let labelSerialized = <@@ let truc = serLabel label labelDelim 
                              truc @@>
    let payloadSerialized = serPayloads args listTypes payloadDelim endDelim 
    <@@ Array.append (%%labelSerialized : byte []) (%%payloadSerialized) @@>



(*let internal readPayload stream (listTypes: string list) =
    let rec aux listPayload (listBytes:byte[] list) =
        match listPayload with
        |[] -> listBytes
        |hd::tl -> let read = readMessage stream
                   let expSize = System.Runtime.InteropServices.Marshal.SizeOf(System.Type.GetType(hd))
                   match read.Length with
                       | n when n=expSize -> aux tl (read::listBytes)
                       | _ -> failwith (sprintf "Payload %s received is not of the expected Type / Size" hd )
    in aux listTypes []*)

(*let internal writeMessage (buf : byte[]) (s : Stream) =
    let dos = new BinaryWriter(s)
    dos.Write(buf)

let internal serializeLabel (typeName : string)  =
    let bytes = ASCIIEncoding.ASCII.GetBytes(typeName)
    let size = bytes.Length |> System.BitConverter.GetBytes
    let message = size |> Array.append <| bytes
    System.Console.WriteLine(System.Text.ASCIIEncoding.ASCII.GetChars(message))
    message

    *)


(*let internal serializeMessage (typeName : string) (listPayload:string list) 
                              (del1: string list) (del2:string list) (del3:string list) (args: Expr list) =
    let mutable buf = [||] //args.Length |> System.BitConverter.GetBytes
    buf <- Array.append buf (typeName |> serializeLabel) 
    buf <- Array.append buf (del1.Head |> toBytes)

(*    let list1 = args
                    |> List.mapi (fun i arg -> 
                                    match listPayload.[i] with
                                        |"System.String" -> 
                                            <@@ let size = ((%%arg:string).Length) 
                                                Type.GetType("System.BitConverter")
                                                    .GetMethod("GetBytes",[|typeof<int>|])
                                                    .Invoke(null,[|box (size:int)|]) |> unbox<byte []> @@>
                                        | _ -> 
                                            <@@ let size = System.Runtime.InteropServices.Marshal.SizeOf(System.Type.GetType(listPayload.[i]))
                                                System.BitConverter.GetBytes(size) |> unbox<byte []> @@> )
                                                *)

    let list1 =  args
                    |> List.mapi (fun i arg -> 
                                    match listPayload.[i] with
                                        |"System.String" | "System.Char"-> 
                                            <@@ Type.GetType("System.Text.UTF8Encoding")
                                                    .GetMethod("GetBytes",[|System.Type.GetType(listPayload.[i])|])
                                                    .Invoke(new System.Text.UTF8Encoding(),[|%%(Expr.Coerce(arg,typeof<obj>))|]) |> unbox<byte []> @@>
                                        | _ -> 
                                        <@@ Type.GetType("System.BitConverter")
                                                .GetMethod("GetBytes",[|System.Type.GetType(listPayload.[i])|])
                                                .Invoke(null,[|%%(Expr.Coerce(arg,typeof<obj>))|] ) |> unbox<byte []> @@> )  

    let list2 = args
                    |> List.mapi (fun i arg -> <@@ del2.Head |> toBytes @@>)

    let listInterm = list1   
                    |> List.fold2 (fun acc f1 f2 -> <@@ Array.append (Array.append (%%acc) (%%f1:byte [])) (%%f2:byte [])  @@>) <@@ buf:byte [] @@> <| list2
    <@@ Array.append (%%listInterm) (del3.Head |> toBytes)  @@>
*)
(*let internal ser (args: Expr list) =
    let lst = args |> List.map(fun arg -> <@@ Type.GetType("System.BitConverter")
                                                .GetMethod("GetBytes",[|typeof<int>|])
                                                .Invoke(null,[|%%(Expr.Coerce(arg,typeof<obj>))|] ) |> unbox<byte []> @@> )

    lst |> List.fold(fun acc f1 -> <@@ Array.append (%%acc) (%%f1:byte []) @@> ) <@@ [||]:byte [] @@>
    
let internal serializeMessage (typeName : string) (listPayload:string list) (args: Expr list) =
    let mutable buf = [||] //args.Length |> System.BitConverter.GetBytes
    buf <- Array.append buf (typeName |> serializeLabel) 

    let list1 = args
                    |> List.mapi (fun i arg -> 
                                    match listPayload.[i] with
                                        |"System.String" -> 
                                            <@@ let size = ((%%arg:string).Length) 
                                                Type.GetType("System.BitConverter")
                                                    .GetMethod("GetBytes",[|typeof<int>|])
                                                    .Invoke(null,[|box (size:int)|]) |> unbox<byte []> @@>
                                        | _ -> 
                                            <@@ let size = System.Runtime.InteropServices.Marshal.SizeOf(System.Type.GetType(listPayload.[i]))
                                                System.BitConverter.GetBytes(size) |> unbox<byte []> @@> )
                                                

    let list2 =  args
                    |> List.mapi (fun i arg -> 
                                    match listPayload.[i] with
                                        |"System.String" | "System.Char"-> 
                                            <@@ Type.GetType("System.Text.UTF8Encoding")
                                                    .GetMethod("GetBytes",[|System.Type.GetType(listPayload.[i])|])
                                                    .Invoke(new System.Text.UTF8Encoding(),[|%%(Expr.Coerce(arg,typeof<obj>))|]) |> unbox<byte []> @@>
                                        | _ -> 
                                        <@@ Type.GetType("System.BitConverter")
                                                .GetMethod("GetBytes",[|System.Type.GetType(listPayload.[i])|])
                                                .Invoke(null,[|%%(Expr.Coerce(arg,typeof<obj>))|] ) |> unbox<byte []> @@> )  


    <@@ buf:byte [] @@> |> List.fold2 (fun acc f1 f2 -> 
                            <@@ (Array.append (%%acc) (%%f1:byte [])) |> Array.append
                                                                      <| (%%f2:byte [])  @@>) 
                        <| list1
                        <| list2
                        *)


let convert (arrayList:byte[] list) (elemTypelist:string list) =
    let rec aux (arrList:byte[] list) (elemList:string list) (acc:obj list)=
        match arrList with
            |[] -> List.rev acc
            |hd::tl ->  let sub = elemList.Head.Split('.')
                        let typing = sub.[sub.Length-1]
                        let mymethod = Type.GetType("System.BitConverter")
                                           .GetMethod("To"+typing,[|typeof<byte []>;typeof<int>|])
                        let invoke = mymethod.Invoke(null,[|box hd;box 0|])
                        aux tl (elemList.Tail) (invoke::acc)
    aux arrayList elemTypelist []
   
let deserialize (args: Expr list)  (listTypes:string list) (messages: _ list) (role:string) =
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]

    <@@ 
        let result = Regarder.receiveMessage "agent" messages role listTypes 
        printfn " received Bytes: %A" result
        let received = (result.Tail |> convert <| listTypes ) |> List.toSeq          
        Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
    @@>
                 

let deserializeAsync (args: Expr list)  (listTypes:string list) (messages: _ list) (role:string) =  
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]
    <@@ 
        let work = 
            async{            
                let! result = Regarder.receiveMessageAsync "agent" messages role listTypes 
                let received = (result.Tail |> convert <| listTypes ) |> List.toSeq          
                Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
            }
        Async.Start(work)
     @@>


let deserializeChoice (args: Expr list) (listTypes:string list) =
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]
    <@@ 
        let result = Regarder.receiveChoice "agent" 
        let received = (result |> convert <| listTypes ) |> List.toSeq          
        //let received = result |> List.toSeq
        Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
    @@>