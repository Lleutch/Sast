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

// TODO : Change when fields will be added + deserialize has to create an object that has getters and setters to be locked until received values (futures + promises)


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


let convert (arrayList:byte[] list) (elemTypelist:string list) = // CHANGE TO (arrayList : byte[] list) (elemTypelist:string list) et iterate through it
    let rec aux (arrList:byte[] list) (elemList:string list) (acc:obj list)=
        match arrList with
            |[] -> List.rev acc
            |hd::tl ->  printfn " PUTINNNNNNNNN DE MERDE"
                        let sub = elemList.Head.Split('.')
                        printfn " CONVERT1 "
                        let typing = sub.[sub.Length-1]
                        printfn " CONVERT2 %s %s " typing ("To"+typing)
                        let mymethod = Type.GetType("System.BitConverter")
                                           .GetMethod("To"+typing,[|typeof<byte []>;typeof<int>|])
                        printfn " CONVERT3 "    
                        let invoke = mymethod.Invoke(null,[|box hd;box 0|])
                        printfn " CONVERT4 "
                        aux tl (elemList.Tail) (invoke::acc)
    aux arrayList elemTypelist []
   
(*let deserialize (args: Expr list) (received:byte[] list) (listTypes: string list)  =  
    (*let truc (arg:Expr) (byteReceived:byte[]) (aType:string) = 
        let result = byteReceived |> convert <| aType
        let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
        let genType = generic.MakeGenericType(System.Type.GetType(aType))
        printfn "J EN SUIS LA DANS DESERIALIZATION %A" result
        genType.GetMethod("SetResult")
               .Invoke(arg,[|result|])
               *)

    let list1 = args |> List.mapi(fun i arg ->
                                    <@@ 
                                        printfn "OULPLADSQDQL jLSDlQKSJD qlskdj %A" (listTypes.[0])
                                        let truc = (received.[i]) |> convert <| (listTypes.[i])
                                        printfn "MMMMMMMMMMMMmmma %A %A %A" (received.[0]) (truc) (listTypes.Length) 
                                        let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
                                        printfn "Antoine"
                                        let genType = generic.MakeGenericType(System.Type.GetType(listTypes.[i]))
                                        printfn "HELOOO"
                                        genType.GetMethod("SetResult")
                                               .Invoke((%%Expr.Coerce(args.[i],typeof<obj>)),[|truc|]) |> ignore
                                        printfn "ESSAYE"
                                     @@> )

    list1 |> List.fold (fun acc fe -> Expr.Sequential(acc,fe) ) <@@ () @@>
    
   
    //let list1 = args
     //              |> List.fold (fun acc f1 -> Expr.Sequential(acc,f1)) <@@ () @@>*)
    

(*let continuer (listTypes:string list) (messages: _ list) (role:string) =
    

    let task = Async.StartAsTask(Regarder.receiveMessage "agent" messages role listTypes)
    task.Wait()
    task.Result*)
    (*let work =async{
                let task = Async.StartAsTask(Regarder.receiveMessage "agent" messages role listTypes)
                task.Wait()
                let (result:byte[]list) = task.Result
                result |> List.iter(fun x -> printfn " I LOVE YOU %A " x)
                //return result
              }
                   
    Async.Start work*)
    //finalResult |> List.iter(fun elem -> printfn " OH YESS I DIIIID !!!!! %A" elem)  

    //Task.Run(fun () -> Async.StartAsTask(Regarder.receiveMessage "agent" [message] role  listPayload).ContinueWith(fun (listTask:Task<byte[] list>) -> printf "OUiIIIIIIIIIIIIII" )) |> ignore
        (*let pierre = async{
            while not(task.IsCompleted) do
                printf "PIERREEEEEEEEEEEEEEEEEEEEEEEEEEEEE %A" (task.IsCompleted)
            printf "NO WAYYYY YOU DIDN'T !!!!!! %A" (task.IsCompleted)
        }
        Async.Start(pierre)
    
        task.ContinueWith(fun (listTask:Task<byte[] list>) ->
                                printf "CONTINUER ????")*)
        
(*let test (args: Expr list) (received:byte[] list) (listTypes: string list)  = 
            let n = listTypes.Length
            printfn "OULPLADSQDQL jLSDlQKSJD qlskdj %A" (listTypes.[0])
            let truc = (received.[0]) |> convert <| (listTypes.[0])
            printfn "MMMMMMMMMMMMmmma %A %A %A" (received.[0]) (truc) (listTypes.Length) 
            let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
            printfn "Antoine"
            let genType = generic.MakeGenericType(System.Type.GetType(listTypes.[0]))
            printfn "HELOOO"
            genType.GetMethod("SetResult")
                   .Invoke(args.[0],[|truc|]) |> ignore
            printfn "ESSAYE" 

            *)
let getBufType (element:string) =
    let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
    generic.MakeGenericType(System.Type.GetType(element))

let deserialize (args: Expr list)  (listTypes:string list) (messages: _ list) (role:string) = //(args: Expr list) = // (receivedTask:Task<byte[] list>) (listTypes: string list) = 
    //let arg = args.Head
    
     // let receivedTask = Task.Run(fun () -> (%%receivedTaskExpr:Task<byte[] list>).ContinueWith(fun (listTask:Task<byte[] list>) ->
    (*let exprReceive = <@@          //                                                                                                  printf "MOIIIIIIIIIIIIIIIIIIIII????"))
                        printf "receivedTask ??????????????????"
                        continuer listTypes messages role
                       @@> // printf "Franchement la ???"    
    let exprDes = deserialize args exprReceive listTypes
    *)
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]
    <@@    let result = Regarder.receiveMessage "agent" messages role listTypes 
           // I HAVE TO CONVERT INTO LIST OF OBJ ( INT32, STRING ) TO BOX WHILE CONVERTING
           let received = (result |> convert <| listTypes ) |> List.toSeq          
           Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
          // Runtime.setResults (Seq.map received) (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) }
    @@>

    (*<@@ let work = async{
            //let result = (%%exprReceive:byte[] list)
            let! result = Regarder.receiveMessage "agent" messages role listTypes
            //deserialize args result listTypes
            //deserialize args result listTypes |> ignore
            //printfn "HEYYYYYYYYYY"
            //test args result listTypes |> ignore
            let n = listTypes.Length
            let rec aux (buf: Expr list) (received:byte[] list) (listPay: string list) =
                printfn "OULPLADSQDQL jLSDlQKSJD qlskdj %A" (listPay.Head)
                let truc = (received.Head) |> convert <| (listPay.Head)
                printfn "MMMMMMMMMMMMmmma %A %A %A" (received.[0]) (truc) (listPay.Length) 
                let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
                printfn "Antoine"
                let genType = generic.MakeGenericType(System.Type.GetType(listPay.Head))
                printfn "HELOOO"
                genType.GetMethod("SetResult")
                       .Invoke((%%Expr.Coerce(buf.Head,typeof<obj>)),[|truc|]) |> ignore
                printfn "ESSAYE"
                aux buf.Tail received.Tail listPay.Tail
            in aux args result listTypes
        } 
        Async.Start(work)
        @@>*)
            (*let result = received |> convert <| listTypes
    let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
    let genType = generic.MakeGenericType(System.Type.GetType(listTypes))
    printfn "J EN SUIS LA DANS DESERIALIZATION %A" result
    genType.GetMethod("SetResult")
           .Invoke(arg,[|result|]) |> ignore     
    *)
//    () 
    //receivedTask.ContinueWith(fun (listTask:Task<byte[] list>) ->
      //                      let received = listTask.Result
        //                    printf " C EST FAHHHHHHHHHHD !!" )
                            (*args |> List.iteri ( fun i arg ->   let result = received.[i] |> convert <| listTypes.[i]
                                                                <@@ let generic = typeof<DomainModel.Buf<_>>.GetGenericTypeDefinition() 
                                                                    let genType = generic.MakeGenericType(System.Type.GetType(listTypes.[i]))
                                                                    printfn "J EN SUIS LA DANS DESERIALIZATION %A" result
                                                                    genType.GetMethod("SetResult")
                                                                           .Invoke((%%arg),[|(%%Expr.Coerce(Expr.Value(result),typeof<obj>))|])  @@> |> ignore ) ) 
                                                                           *)
                                                                          // @@>
   (* let list1 = args |> List.mapi (fun i arg -> <@@ //let buffer = (%%buf:Async<byte[] list>)
                                                    let mutable buffer = []
                                                    System.Console.WriteLine("DESERIALIZE +ezrzear")
                                                    let received = (%%buf:Async<byte[] list>)
                                                    System.Console.WriteLine("DESERIALIZE +zerzerze fsdfqsf sqd sdf qd1")
                                                    async{
                                                        let! result = received |> Async.Catch
                                                        System.Console.WriteLine("DESERIALIZE +11111111")
                                                        match result with
                                                            |Choice1Of2 c1 ->   buffer <- c1
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
                                                            |Choice2Of2 c2 -> failwith (sprintf "Mistake during deserialization : %s" c2.Message)
                                                    }     
                                                        (*buffer <- received
                                                    
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
                                                            .Invoke((%%Expr.Coerce(arg,typeof<obj>)),[|result|]) |> ignore*)
                                                         
                                                     @@> )
    list1 
        |> List.fold (fun acc f1 -> Expr.Sequential(acc,f1)) <@@ () @@>
    *)               
                 

let deserializeAsync (args: Expr list)  (listTypes:string list) (messages: _ list) (role:string) = //(args: Expr list) = // (receivedTask:Task<byte[] list>) (listTypes: string list) = 
    //let arg = args.Head
    
     // let receivedTask = Task.Run(fun () -> (%%receivedTaskExpr:Task<byte[] list>).ContinueWith(fun (listTask:Task<byte[] list>) ->
    (*let exprReceive = <@@          //                                                                                                  printf "MOIIIIIIIIIIIIIIIIIIIII????"))
                        printf "receivedTask ??????????????????"
                        continuer listTypes messages role
                       @@> // printf "Franchement la ???"    
    let exprDes = deserialize args exprReceive listTypes
    *)
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]
    <@@ let work = async{            
           let! result = Regarder.receiveMessageAsync "agent" messages role listTypes 
           // I HAVE TO CONVERT INTO LIST OF OBJ ( INT32, STRING ) TO BOX WHILE CONVERTING
           let received = (result |> convert <| listTypes ) |> List.toSeq          
           Runtime.setResults received (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) }
          // Runtime.setResults (Seq.map received) (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) }
    Async.Start(work) @@>



 
// TODO : TEST IF CORRECT 
let deserializeMessage (args: Expr list) (buf: Expr) (listTypes: string list) = 
    let list1 = args |> List.mapi (fun i arg -> <@@ //let buffer = (%%buf:Async<byte[] list>)
                                                    let mutable buffer = []
                                                    System.Console.WriteLine("DESERIALIZE +ezrzear")
                                                    let received = (%%buf:Async<byte[] list>)
                                                    System.Console.WriteLine("DESERIALIZE +zerzerze fsdfqsf sqd sdf qd1")
                                                    async{
                                                        let! result = received |> Async.Catch
                                                        System.Console.WriteLine("DESERIALIZE +11111111")
                                                        match result with
                                                            |Choice1Of2 c1 ->   buffer <- c1
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
                                                            |Choice2Of2 c2 -> failwith (sprintf "Mistake during deserialization : %s" c2.Message)
                                                    }     
                                                        (*buffer <- received
                                                    
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
                                                            .Invoke((%%Expr.Coerce(arg,typeof<obj>)),[|result|]) |> ignore*)
                                                         
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
