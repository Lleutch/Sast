module GenerativeTypeProviderExample.IO

open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Net.Sockets
open System.IO
open System.Text
open System
open Microsoft.FSharp.Quotations
open GenerativeTypeProviderExample.DomainModel

// TODO : Change when fields will be added + deserialize has to create an object that has getters and setters to be locked until received values (futures + promises)

let internal readMessage (s : Stream) =
    let dis = new BinaryReader(s)
    let size = dis.ReadInt32()
    dis.ReadBytes(size)


let internal readPayload stream (listTypes: string list) =
    let rec aux listPayload (listBytes:byte[] list) =
        match listPayload with
        |[] -> listBytes
        |hd::tl -> let read = readMessage stream
                   let expSize = System.Runtime.InteropServices.Marshal.SizeOf(System.Type.GetType(hd))
                   match read.Length with
                       | n when n=expSize -> aux tl (read::listBytes)
                       | _ -> failwith (sprintf "Payload %s received is not of the expected Type / Size" hd )
    in aux listTypes []

let internal writeMessage (buf : byte[]) (s : Stream) =
    let dos = new BinaryWriter(s)
    dos.Write(buf)

let internal serialize (typeName : string)  =
    let bytes = ASCIIEncoding.ASCII.GetBytes(typeName)
    let size = bytes.Length |> System.BitConverter.GetBytes
    let message = size |> Array.append <| bytes
    System.Console.WriteLine(System.Text.ASCIIEncoding.ASCII.GetChars(message))
    message


let internal serializeMessage (typeName : string) (listPayload:string list) (args: Expr list) =
    let mutable buf = args.Length |> System.BitConverter.GetBytes
    buf <- Array.append buf (typeName |> serialize)

    let list1 = args
                    |> List.mapi (fun i arg -> match listPayload.[i] with
                                                    |"System.String" -> <@@ let size = ((%%arg:string).Length) 
                                                                            Type.GetType("System.BitConverter")
                                                                                .GetMethod("GetBytes",[|typeof<int>|])
                                                                                .Invoke(null,[|box (size:int)|]) |> unbox<byte []> @@>
                                                    | _ -> <@@ let size = System.Runtime.InteropServices.Marshal.SizeOf(System.Type.GetType(listPayload.[i]))
                                                               System.BitConverter.GetBytes(size) |> unbox<byte []> @@> )

    let list2 = args
                    |> List.mapi (fun i arg -> match listPayload.[i] with
                                                    |"System.String" | "System.Char"-> <@@ Type.GetType("System.Text.ASCIIEncoding")
                                                                                               .GetMethod("GetBytes",[|System.Type.GetType(listPayload.[i])|])
                                                                                               .Invoke(new System.Text.ASCIIEncoding(),[|%%(Expr.Coerce(arg,typeof<obj>))|]) |> unbox<byte []> @@>
                                                    | _ -> <@@ Type.GetType("System.BitConverter")
                                                                   .GetMethod("GetBytes",[|System.Type.GetType(listPayload.[i])|])
                                                                   .Invoke(null,[|%%(Expr.Coerce(arg,typeof<obj>))|] ) |> unbox<byte []> @@> )  

    list1   
        |> List.fold2 (fun acc f1 f2 -> <@@ Array.append (Array.append (%%acc) (%%f1:byte [])) (%%f2:byte [])  @@>) <@@ buf:byte [] @@> <| list2
    
    

    
// TODO : TEST IF CORRECT 
let deserializeMessage (args: Expr list) (buf: Expr) (listTypes: string list) = 
    let list1 = args |> List.mapi (fun i arg -> <@@ //let buffer = (%%buf:Async<byte[] list>)
                                                    let mutable buffer = []
                                                    let received = (%%buf:byte[] list)
                                                    buffer <- received
                                                    
                                                    //Async.RunSynchronously(truc)
                                                    let read = buffer.[i]
                                                    printfn "DESERIALIZED MESSAGE : %A %d " (Array.toList read) (buffer.Length)
                                                    let currentType = listTypes.Head
                                                    let sub = currentType.Split('.')
                                                    let typing = sub.[sub.Length-1]
                                                  //  printfn "ICI VOYONS VOIR "
                                                    let result = Type.GetType("System.BitConverter")
                                                                        .GetMethod("To"+typing,[|typeof<byte []>;typeof<int>|])
                                                                        .Invoke(null,[|box read;box 0|])
                                                        //let result = box 5
                                                    //printfn "PEUT ETRE %A" result
                                                    let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
                                                    let genType = generic.MakeGenericType(System.Type.GetType(currentType))
                                                    //printfn "J EN SUIS LA DANS DESERIALIZATION %A" result
                                                    genType.GetMethod("SetResult")
                                                        .Invoke((%%Expr.Coerce(arg,typeof<obj>)),[|result|]) |> ignore
                                                            
                                                     @@> )
    list1 
        |> List.fold (fun acc f1 -> Expr.Sequential(acc,f1)) <@@ () @@>
                   
    
    
    (*<@@ printfn "APELLEEEEEEEEEEE %A %A" args.Length (%%buf:byte[] list).Length @@> |> ignore
    match args with
        |[] -> <@@ printfn "MAIIIIIIIIIIIIIIIRDE" @@>
        |hd::tl -> <@@ let buffer = (%%buf:byte[] list)
                       let read = buffer.Head
                       printfn "DESERIALIZED MESSAGE : %A" (Array.toList read)
                       let currentType = listTypes.Head
                       let sub = currentType.Split('.')
                       let typing = sub.[sub.Length-1]
                       let result = Type.GetType("System.BitConverter")
                                      .GetMethod("To"+typing,[|typeof<byte []>|])
                                      .Invoke(null,[|box read|])
                                              
                       let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
                       let genType = generic.MakeGenericType(System.Type.GetType(currentType))

                       genType.GetMethod("SetResult")
                              .Invoke((%%hd),[|result|]) |> ignore
                   
                       deserializeMessage (<@@ buffer.Tail @@>) (listTypes.Tail) tl @@>*)


(*let recupBuf (args: Expr list) =
    let rec aux (listArgs: Expr list) (listBuf: Buf<_> list) =
        match listArgs with
            |[] -> args.Head
            |hd::tl -> let mutable buffer = listBuf
                       <@@ let buf = (%%hd)
                           buffer <- buf::buffer @@>
    in aux args []*)
