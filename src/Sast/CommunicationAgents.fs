module ScribbleGenerativeTypeProvider.CommunicationAgents


// Outside namespaces and modules
open System.Net.Sockets
open System.IO
open System.Net
open System.Text
open System
// ScribbleProvider specific namespaces and modules
open ScribbleGenerativeTypeProvider.DomainModel
open System.Collections.Generic

type VarCache() =
    let data = Dictionary<string,int>()
    member x.RuntimeOperation() = data.Count  
      
    member x.Add(k:string, v:int) = 
        match data.ContainsKey(k) with 
        | true -> data.Item(k) <- v
        | _ -> data.Add(k, v) 
    
    member x.Get(k:string) = 
        match data.TryGetValue(k) with 
        | true, value -> value
        | _ -> failwith (sprintf "Cannot retrieve value from cache: %s" k)


let createCache = new VarCache()

let printing message data =
    let doPrinting = true
    if doPrinting then
        printfn "%s %A" message data

exception TooManyTriesError of string

let internal moreLists (labels:byte[] list) =
    let rec aux acc (list1 : byte[] list) =
        match list1 with
            |[] -> acc
            |hd::tl -> printing "LOOP :" hd
                       let encode = new System.Text.UTF8Encoding()
                       printing  "LOOP : Encoding :" encode
                       // TODO : FIX Num 1 TMP
                       let str1 = encode.GetString(hd.[0..hd.Length-2])
                       let str2 = encode.GetString(hd)
                       printing  "LOOP : GetString : " (str1,str2)
                       printing  "MAP : " DomainModel.mappingDelimitateur
                       let tryDelims1 = DomainModel.mappingDelimitateur.TryFind str1
                       let tryDelims2 = DomainModel.mappingDelimitateur.TryFind str2
                       let listDelim,_,_ =
                           match tryDelims1, tryDelims2 with
                           | None , Some delims -> delims
                           | Some delims , None -> delims
                           | _ , _ -> failwith (sprintf "Error with delimitations : For the moment avoid to have a label equal to a label + delimiter \n Example : %s AND %s" str1 str2)
                       printfn "LOOP : Get Delims"
                       aux (listDelim::acc) tl
    printing  "Labels :" labels
    aux [] labels

let internal isInOne (weirdList: string list list) (str:string) =
    let rec aux (list1: string list list) =
        match list1 with
            |[] -> false
            |hd::tl -> if (List.exists (fun elem -> elem = str) hd) then
                           true
                       else
                           aux tl
    aux weirdList

let internal readLab (s : NetworkStream) (labels : byte[] list) =
    printing  "Inside read Label function" ""
    let listsDelim = moreLists labels
    printing  "getting delimiters" ""
    let decode = new UTF8Encoding()
    printing  "getting delimiters and encoding" ""
    let dis = new BinaryReader(s)
    printing  "Reading Label :" ""
    let rec aux acc = 
        printing  "Aux :" (acc,s.DataAvailable,s.ReadTimeout,s)
        let tmp = dis.ReadByte()
        printing  "Aux : Read byte :" tmp
        let value = decode.GetString([|tmp|])
        printing "Aux : Decode :" value
        printing value tmp
        printing "Aux : After printing" ""
        if (isInOne listsDelim value) then
            // TODO : FIX Num 2 TMP
            (acc,[|tmp|])
        else
            aux (Array.append acc [|tmp|])
    printing  "Read Real Label :" ""
    aux [||]

let readPay (s:Stream) (label:string) = 
    // TODO : FIX Num 3 TMP
    let str1 = label.[0..(label.Length-2)] 
    let str2 = label
    let tryDelims1 = DomainModel.mappingDelimitateur.TryFind str1
    let tryDelims2 = DomainModel.mappingDelimitateur.TryFind str2
    printing "Reading payloads : Mapping Delims = %A" (DomainModel.mappingDelimitateur,str1,str2) 
    let _,listDelPay,listDelEnd = 
        match tryDelims1, tryDelims2 with
        | None , Some delims -> delims
        | Some delims , None -> delims
        | _ , _ -> failwith (sprintf "Error with delimitations : For the moment avoid to have a label equal to a label + delimiter \n Example : %s AND %s" str1 str2)

    let dis = new BinaryReader(s)
    let decode = new UTF8Encoding()
    printing "Reading payloads :" ""
    let rec aux accList accArray =
        let tmp = dis.ReadByte()
        let value = decode.GetString([|tmp|])
        printing value tmp
        if (List.exists (fun elem -> elem = value) listDelEnd) then 
            (accArray::accList) |> List.rev 
        elif (List.exists (fun elem -> elem = value) listDelPay) then 
            aux (accArray::accList) [||]
        else
            aux accList (Array.append accArray [|tmp|])
    in aux [] [||]


type AgentSender(ipAddress,port) =

    let waitSynchronously timeout =
        async{
            do! Async.Sleep(timeout*1000)
        } 
    
    // FEATURE to add: 5 Tries of 3 seconds and then double the time at each try following Microsoft Standards.
    // FEATURE ADDED
    let connect address p (tcpClient:TcpClient) =
        System.Console.WriteLine("Connecting ...")
        let rec aux timeout count =
            let tries = 5
            try
                match count with
                    |n when n<tries ->  System.Console.WriteLine("Attempt number : {0} out of {1} : waiting {2} seconds before restarting...",
                                                                  count,tries,timeout)
                                        //MOCK THE CONNECTION HERE
                                        System.Console.WriteLine("Trying to connect to : IP = {0}  and Port = {1} ...", IPAddress.Parse(address), p)
                                        tcpClient.Connect(IPAddress.Parse(address),p)
                                        System.Console.WriteLine("Apres essai ...")
                                        if (tcpClient.Connected) then
                                            System.Console.WriteLine("Connected to: IP = {0}  and Port = {1} ...", IPAddress.Parse(address), p)                                            
                    |_ -> tcpClient.Connect(IPAddress.Parse(address),p)
                          if not(tcpClient.Connected) then
                              raise (TooManyTriesError("You have tried too many times to connect, the partner is not ready to connect with you"))
            with
                | :? System.ArgumentException as ex -> printfn "Argument Exception: %s"  ex.Message
                | :? System.Net.Sockets.SocketException as ex ->  printfn "Socket Exception error code: %d"  ex.ErrorCode
                                                                  timeout |> waitSynchronously |> Async.RunSynchronously
                                                                  aux (timeout*2) (count+1)
                | :? System.ObjectDisposedException as ex -> printfn "Object Disposed Exception: %s"  ex.Message
                | TooManyTriesError(str) -> printfn "Too Many Tries Error: %s" str
        
        in aux 3 0

    let send (stream:NetworkStream) (actor:Agent<Message>) =
        let rec loop () = async {
            let! msg = actor.Receive()
            match msg with
                |ReceiveMessageAsync _ ->
                    ()
                    return! loop()
                |ReceiveMessage _ ->
                    () // Raise an exception Error due to bad coding in the type provider
                    return! loop()      
                |SendMessage (message,role) -> // The serialization is done before
                    printing "Send Message :" (Array.toList message)
                    do! stream.AsyncWrite(message)
                    printing "Message Sent via TCP" ""
                    return! loop()
            }
        in loop()
 
    let mutable agentSender = None 

    member this.SendMessage(message) =
        printing "Send Message : About to send" ""
        match (agentSender:Option<Agent<Message>>) with
            |None -> () // Raise an exception Error due to using this method before the Start method in the type provider 
            |Some sending -> 
                printing "Send Message : post to actor loop" ""
                sending.Post(Message.SendMessage message)

    member this.Start() = // Raise an exception due to trying to connect and parsing the IPAddress
        let tcpClientSend = new TcpClient()
        connect ipAddress port tcpClientSend
        let stream = tcpClientSend.GetStream()    
        agentSender <- Some (Agent.Start(send stream))



type AgentReceiver(ipAddress,port) =

    let server = new TcpListener(IPAddress.Parse(ipAddress),port)
    let mutable clientMap = Map.empty
 
    let rec waitForCancellation str count =
        match count with
            |n when n=0 -> ()
            |n when n>0 -> if not(clientMap.ContainsKey str) then
                                System.Console.WriteLine(" I AM WAITING FOR CANCELLATION !!!!")
                                Async.RunSynchronously(Async.Sleep 100)
                                waitForCancellation str (count-1)
                           else
                                ()
            |_ -> ()
        
 
    let binding (tcpListenerReceive:TcpListener) (actor:Agent<Message>) =
        let rec loop () = async {
            let client = tcpListenerReceive.AcceptTcpClient()
            let stream = client.GetStream()
            // CHANGE BELOW BY READING THE ROLE IN ANOTHER Map<role:string,(IP,PORT)>
            let readRole = "hey"//readAllBytes stream
            // CHANGE ABOVE BY READING THE ROLE IN ANOTHER Map<role:string,(IP,PORT)>
            clientMap <- clientMap.Add(readRole,stream)
            printing " SIZE :" (clientMap.Count,readRole)
            return! loop()
            }
        in loop()

    let isIn (aList:_ list) x = 
        let rec aux list x In =
            match list with
                |[] -> In
                |hd :: tl -> match hd with
                                |n when n=x -> true
                                | _ -> aux tl x In
        in aux aList x false
 
    let receive (actor:Agent<Message>) =
        let rec loop () = async {
            let! msg = actor.Receive()
            match msg with
                |SendMessage (message,role)->
                    ()  // Raise an exception Error due to bad coding in the type provider
                    return! loop()      
                |ReceiveMessageAsync (message,role,listTypes,channel) -> // The UnMarshalling is done outside the Agent Routing Architecture NOT HERE.
                    let fakeRole = "hey"
                    if not(clientMap.ContainsKey(fakeRole)) then
                        waitForCancellation fakeRole 50 |> ignore // Change th number
                    let stream = clientMap.[fakeRole]
                    // DESERIALIZER BIEN LA
                    let decode = new System.Text.UTF8Encoding()
                    let (label,delim) = readLab stream message
                    match label with
                        |msg when (message |> isIn <| (Array.append msg delim) ) |> not -> failwith "Received a wrong Label, that doesn't belong to the possible Labels at this state"
                        | _ ->  let payloads = readPay stream (decode.GetString(label))
                                let list1 = label::payloads
                                channel.Reply(list1)
                    return! loop()
                |ReceiveMessage (message,role,listTypes,channel) ->
                    printing "Check ClientMap :" clientMap
                    if not(clientMap.ContainsKey("hey")) then
                        waitForCancellation "hey" 50 |> ignore // Change the number
                    let stream = clientMap.["hey"]
                    // DESERIALIZER BIEN LA
                    let decode = new System.Text.UTF8Encoding()
                    printing "Wait Read Label" stream.DataAvailable
                    let mutable succeed = false
                    let (label,delim) = 
                        try 
                            printing "INSIDE the TRY WITH for readLab:" message
                            let res = readLab stream message
                            succeed <- true
                            res
                        with
                        | e -> 
                            succeed <- false
                            [||],[||]
                    printing " Label Read :" (label,delim,message,succeed)
                    match label with
                    |msg when (message |> isIn <| (Array.append msg delim) ) |> not -> 
                        printing "wrong label read :" (label,message)
                        failwith "Received a wrong Label, that doesn't belong to the possible Labels at this state"
                    | _ ->  printing "Read Payload" ""
                            let payloads = readPay stream (decode.GetString(label))
                            let list1 = label::payloads
                            printing "Before Reply on channel" ""
                            channel.Reply(list1)
                    (*let dis = new BinaryReader(stream)
                    let c = dis.ReadBytes(4)
                    channel.Reply([c])*)
                    return! loop()
                    
            }
        in loop()
 
    let mutable agentReceiver = None

    do 
        printing "Current Address IP :" (IPAddress.Parse(ipAddress),ipAddress,port)

   
    member this.Start()=
        server.Start()
        System.Console.WriteLine("TCP LISTENER DE AGENT RECEIVER START...")
        Agent.Start(binding server) |> ignore
        agentReceiver <- Some (Agent.Start(receive))

    // To Close the listener to optimize ressource usage and avoid receiving things that do not belong to the protocol.
    // To be done in the finish ProvidedMethod that represent the Ending process in Session Types.
    member this.Stop() =
        for client in clientMap do
            client.Value.Close()
        server.Stop()

    // Be carefull with this function: IF IT'S NONE RAISE AN EXCEPTION
    member this.ReceiveMessageAsync(message) =
        System.Console.WriteLine("RECEIVING...")
        match agentReceiver with
            |Some receive -> receive.Post(Message.ReceiveMessageAsync message )                            
            |None -> failwith " agent not instanciated yet"
                     
    member this.ReceiveMessage(message) =
        System.Console.WriteLine("RECEIVING...")
        let (msg,role,listTypes,ch) = message
        match agentReceiver with
            |Some receive -> 
                printfn "Wait Reply"
                receive.PostAndReply(fun channel -> Message.ReceiveMessage (msg,role,listTypes,channel))
            |None -> failwith " agent not instanciated yet"
                     

type AgentRouter(agentMap:Map<string,AgentSender>,agentReceiving:AgentReceiver) =
    
    let agentMapping = agentMap
    let agentReceiver = agentReceiving

    let mutable (payloadChoice:byte[] list) = []
 
    let sendAndReceive (agentRouter:Agent<Message>) =
        let rec loop () = async{
            let! msg = agentRouter.Receive()
            match msg with
                |SendMessage (message,role) ->
                    printing "SendMessage : Agent Mapping = " agentMapping  
                    let agentSender = agentMapping.[role]
                    printing "Got the correct agent" ""
                    agentSender.SendMessage(message,role) // Here, message is the serialized message that needs to be sent
                    printing "Sent the message to the correct the agent that will use tcp" ""
                    return! loop()
                |ReceiveMessageAsync (message,role,listTypes,channel) -> 
                    printing "Receives Message Async : send to Agent" ""
                    agentReceiver.ReceiveMessageAsync(message,role,listTypes,channel) // Be Carefull: message is the serialized version of the Type
                                                                                           // While replyMessage is the message really received from the network 
                    return! loop()
                |ReceiveMessage (message,role,listTypes,channel) -> 
                    printing "Receives Message : send to Agent" ""
                    let message = agentReceiver.ReceiveMessage(message,role,listTypes,channel) // Be Carefull: message is the serialized version of the Type
                    printing "Receives Message : reply to channel" ""
                    channel.Reply(message)                                                                                   // While replyMessage is the message really received from the network 
                    printing "Receives Message : replied to channel" ""
                    return! loop()
            }
        in loop()
   
    let agentRouter = Agent.Start(sendAndReceive)
 
    member this.Start() =
        agentReceiver.Start()
        for sender in agentMapping do
            sender.Value.Start()
               
    member this.SendMessage(message) =
        printing "SendMessage : Post to the write role = " message
        agentRouter.Post(Message.SendMessage message)
   
    member this.ReceiveMessage(message) =
        let (msg,role,listTypes) = message
        let replyMessage = agentRouter.PostAndReply(fun channel -> Message.ReceiveMessage (msg,role,listTypes,channel))
        payloadChoice <- replyMessage.Tail
        replyMessage

    member this.ReceiveMessageAsync(message) = 
        let (msg,role,listTypes) = message
        let replyMessage = agentRouter.PostAndAsyncReply(fun channel -> Message.ReceiveMessageAsync (msg,role,listTypes,channel))
        replyMessage            

    member this.ReceiveChoice() =
        printing "Go through A choice!!!" ""
        payloadChoice
        




// Functions that generate the agents.

let isIn (list:string list) (localRole:string) =
    list |> List.exists (fun x -> x=localRole) 

let private createReceiver (ipAddress:string) (port:int)=
    new AgentReceiver(ipAddress,port)

let createMapSender (partnersInfo: IList<ConfigFile.Partners_Item_Type>) (listRoles:string list) =
    let mutable mapping = Map.empty<string,AgentSender>
    for partner in partnersInfo do
        match (listRoles |> isIn <| partner.Name) with
            | false -> failwith (sprintf "The following role : %s from the config file doesnt belong to the protocol: 
                                 Check If you have spelled it correctly, be aware, the role is case-sensitive"  (partner.Name) )
            | true -> mapping <- mapping.Add(partner.Name, new AgentSender(partner.IP,partner.Port))
    mapping

let createRouter (configInfos:ConfigFile) (listRoles:string list) =
    let lengthList = listRoles.Length
    let configSize = configInfos.Partners.Count + 1
    match (configSize = lengthList) with
    | false -> failwith "you don't have the correct number of roles in the YAML Configuration file"
    | true ->
        match (listRoles |> isIn <| configInfos.LocalRole.Name) with
        |false -> failwith (sprintf "The following local role : %s from the config file doesn't belong to the protocol: 
                            Check If you have spelled it correctly, be aware, the role is case-sensitive"  (configInfos.LocalRole.Name) )
        |true -> 
            printfn "Agents Infos :"            
            let mapAgentSender = configInfos.Partners |> createMapSender <| listRoles
            let agentReceiver = configInfos.LocalRole.IP |> createReceiver <| configInfos.LocalRole.Port
            printfn "Infos For Agent Sender : %A" (mapAgentSender,configInfos.Partners,listRoles)
            printfn "Infos For Agent Receiver : %A" (configInfos.LocalRole.IP,configInfos.LocalRole.Port,configInfos.LocalRole.Name)
            new AgentRouter(mapAgentSender,agentReceiver)