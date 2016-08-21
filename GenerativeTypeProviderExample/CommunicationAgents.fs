module GenerativeTypeProviderExample.CommunicationAgents


// Outside namespaces and modules
open System.Net.Sockets
open System.IO
open System.Net
open System.Text
open System
// ScribbleProvider specific namespaces and modules
open GenerativeTypeProviderExample.DomainModel
open System.Collections.Generic


exception TooManyTriesError of string

let internal readMessage (s : Stream) =
    let dis = new BinaryReader(s)
    let size = dis.ReadInt32()
    dis.ReadBytes(size)

let internal readPayload (s : Stream) (payloadType : string) =
    let dis = new BinaryReader(s)
    let sizeExpected = System.Runtime.InteropServices.Marshal.SizeOf(System.Type.GetType(payloadType))
    let sizeReal = dis.ReadInt32()
    match (sizeExpected = sizeReal) with
        | false -> failwith (sprintf "Expected a payload of type %s but received something different" payloadType) 
        | true -> dis.ReadBytes(sizeReal)

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
            System.Console.WriteLine("THE SENDING METHOD HASN'T BEEN CALLED YET")
            let! msg = actor.Receive()
            match msg with
                |ReceiveMessageAsync _ ->
                    ()
                    return! loop()
                |ReceiveMessage _ ->
                    () // Raise an exception Error due to bad coding in the type provider
                    return! loop()      
                |SendMessage (message,role) -> // The serialization is done before
                    printfn "SENDING THE MESSAGE : %A" (Array.toList message)
                    do! stream.AsyncWrite(message)
                    return! loop()
            }
        in loop()
 
    let mutable agentSender = None 

    member this.SendMessage(message) =
        System.Console.WriteLine("SENDINGGGGGGGGGGGG...")
        match (agentSender:Option<Agent<Message>>) with
            |None -> () // Raise an exception Error due to using this method before the Start method in the type provider 
            |Some sending -> sending.Post(Message.SendMessage message)

    member this.Start() = // Raise an exception due to trying to connect and parsing the IPAddress
        let tcpClientSend = new TcpClient()
        System.Console.WriteLine("TCP CLIENT DE AGENT RECEIVER START...")
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
            System.Console.WriteLine("AGENT RECEIVER ATTEND DES CONNECTIONS...: LISTENING PORTS : {0}",clientMap.Count)
            let client = tcpListenerReceive.AcceptTcpClient()
            System.Console.WriteLine("AGENT RECEIVER A RECU UNE TENTATIVE DE CONNECTION...:{0}",client.Client.RemoteEndPoint)
            let stream = client.GetStream()
            // CHANGE BELOW BY READING THE ROLE IN ANOTHER Map<role:string,(IP,PORT)>
            let readRole = "hey"//readAllBytes stream
            // CHANGE ABOVE BY READING THE ROLE IN ANOTHER Map<role:string,(IP,PORT)>
            clientMap <- clientMap.Add(readRole,stream)
            System.Console.WriteLine("EST CE QUE SA CONTIENT MTN ????? ... {0} {1} {2} ", clientMap.Count ,clientMap.ContainsKey(readRole), readRole)
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
            System.Console.WriteLine("AGENT RECEIVER ATTEND LES MESSAGES...")
            let! msg = actor.Receive()
            System.Console.WriteLine("AGENT RECEIVER A RECUE UN MESSAGE...")
            match msg with
                |SendMessage (message,role)->
                    ()  // Raise an exception Error due to bad coding in the type provider
                    return! loop()      
                |ReceiveMessageAsync (message,role,listTypes,channel) -> // The UnMarshalling is done outside the Agent Routing Architecture NOT HERE.
                    //let stream = clientMap.[role]
                    System.Console.WriteLine("AGENT RECEIVER CHERCHE DANS CLIENTMAP... {0} {1} {2}", clientMap.Count ,clientMap.ContainsKey("hey"),role)
                    waitForCancellation "hey" 50 |> ignore // Change th number
                    if not(clientMap.ContainsKey("hey")) then
                        waitForCancellation "hey" 1 |> ignore // Change th number
                    (*if not(clientMap.ContainsKey("hey")) then
                        System.Console.WriteLine("LA SA PASSE C'EST SUR ET CERTAIN")
                        Async.RunSynchronously(waitForCancellation "hey" 10)//,cancellationToken=cts.Token) *)
                    System.Console.WriteLine("JE CROIS QUE J'EN SUIS LA ...")
                    let stream = clientMap.["hey"]
                    System.Console.WriteLine("AGENT RECEIVER VA LIRE UN MESSAGE {0}...", clientMap.ContainsKey("hey"))
                    let read = readMessage stream
                    printf "MESSAGE LU :%s  MESSAGES ATTENDU:" (System.Text.ASCIIEncoding.ASCII.GetString(read))
                    message |> Seq.iter (fun x -> printf "%s " (System.Text.ASCIIEncoding.ASCII.GetString(x)) )
                    match read with
                        |msg when (message |> isIn <| msg) |> not -> failwith "Received a wrong Label, that doesn't belong to the possible Labels at this state"
                        | _ -> let buffer = [yield read
                                             for elem in listTypes do
                                                yield (readPayload stream elem)]
                               channel.Reply(buffer) 
                    return! loop()
                |ReceiveMessage (message,role,listTypes,channel) ->
                    System.Console.WriteLine("AGENT RECEIVER CHERCHE DANS CLIENTMAP... {0} {1} {2}", clientMap.Count ,clientMap.ContainsKey("hey"),role)
                    waitForCancellation "hey" 50 |> ignore // Change th number
                    if not(clientMap.ContainsKey("hey")) then
                        waitForCancellation "hey" 1 |> ignore // Change th number
                    (*if not(clientMap.ContainsKey("hey")) then
                        System.Console.WriteLine("LA SA PASSE C'EST SUR ET CERTAIN")
                        Async.RunSynchronously(waitForCancellation "hey" 10)//,cancellationToken=cts.Token) *)
                    System.Console.WriteLine("JE CROIS QUE J'EN SUIS LA ...")
                    let stream = clientMap.["hey"]
                    System.Console.WriteLine("AGENT RECEIVER VA LIRE UN MESSAGE {0}...", clientMap.ContainsKey("hey"))
                    let read = readMessage stream
                    printf "MESSAGE LU :%s  MESSAGES ATTENDU:" (System.Text.ASCIIEncoding.ASCII.GetString(read))
                    message |> Seq.iter (fun x -> printf "%s " (System.Text.ASCIIEncoding.ASCII.GetString(x)) )
                    match read with
                        |msg when (message |> isIn <| msg) |> not -> failwith "Received a wrong Label, that doesn't belong to the possible Labels at this state"
                        | _ -> let buffer = [yield read
                                             for elem in listTypes do
                                                yield (readPayload stream elem)]
                               channel.Reply(buffer) 
                    return! loop()
                    
            }
        in loop()
 
    let mutable agentReceiver = None
   
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
        //let (msg,role,listTypes,channel) = message
        
        match agentReceiver with
            |Some receive -> System.Console.WriteLine("I HAVE AN AGENT RECEIVER...") 
                             receive.Post(Message.ReceiveMessageAsync message )
                             //System.Console.WriteLine("NaaaaaaaNAAAAAAAAAAAAAnaaaaaaaaaa!!!!")
                             //return replyMessage
                             (*   
                             let! replyMessage = receive.PostAndAsyncReply(fun newChannel -> Message.ReceiveMessage (msg,newChannel,role))
                             System.Console.WriteLine("NaaaaaaaNAAAAAAAAAAAAAnaaaaaaaaaa!!!!")
                             return replyMessage *)
                             //channel.Reply(replyMessage)
                            
            |None -> ()
                     //async{
                     //   ()
                        //let label,_,_ = message
                        //return label
                     //}        
    member this.ReceiveMessage(message) =
        System.Console.WriteLine("RECEIVING...")
        //let (msg,role,listTypes,channel) = message
        let (msg,role,listTypes,ch) = message
        match agentReceiver with
            |Some receive -> System.Console.WriteLine("I HAVE AN AGENT RECEIVER...") 
                             let replyMessage = receive.PostAndReply(fun channel -> Message.ReceiveMessage (msg,role,listTypes,channel))
                             //receive.Post(Message.ReceiveMessage message )
                             replyMessage
                             //System.Console.WriteLine("NaaaaaaaNAAAAAAAAAAAAAnaaaaaaaaaa!!!!")
                             //return replyMessage
                             (*   
                             let! replyMessage = receive.PostAndAsyncReply(fun newChannel -> Message.ReceiveMessage (msg,newChannel,role))
                             System.Console.WriteLine("NaaaaaaaNAAAAAAAAAAAAAnaaaaaaaaaa!!!!")
                             return replyMessage *)
                             //channel.Reply(replyMessage)
                            
            |None -> failwith " On verra plus tard"
                     //async{
                     //   ()
                        //let label,_,_ = message
                        //return label
                     //}     





type AgentRouter(agentMap:Map<string,AgentSender>,agentReceiving:AgentReceiver) =
    
    let agentMapping = agentMap
    let agentReceiver = agentReceiving

    let mutable (payloadChoice:byte[] list) = []
 
    let sendAndReceive (agentRouter:Agent<Message>) =
        let rec loop () = async{
            System.Console.WriteLine("JE PASSE PAR ICI !")
            let! msg = agentRouter.Receive()
            System.Console.WriteLine(" ET ENSUITE PAR LA !")
            match msg with
                |SendMessage (message,role) ->
                    System.Console.WriteLine("AGENT ROUTER TEST : Route Message to agentSender, partner = {0} ", role)
                    let agentSender = agentMapping.[role]
                    agentSender.SendMessage(message,role) // Here, message is the serialized message that needs to be sent
                    return! loop()
                |ReceiveMessageAsync (message,role,listTypes,channel) -> 
                    System.Console.WriteLine("AGENT ROUTER TEST : Route Message to agentReceiver ASynchrone, partner = {0} ", role)
                    //let! replyMessage = agentReceiver.ReceiveMessage(message,channel,role) // Be Carefull: message is the serialized version of the Type
                    agentReceiver.ReceiveMessageAsync(message,role,listTypes,channel) // Be Carefull: message is the serialized version of the Type
                                                                                           // While replyMessage is the message really received from the network 
                    //channel.Reply(replyMessage)
                    System.Console.WriteLine("ALRIGHTTTTT  AAAASYNC ! ")//{0}", System.Text.ASCIIEncoding.ASCII.GetString(replyMessage))
                    return! loop()
                |ReceiveMessage (message,role,listTypes,channel) -> 
                    System.Console.WriteLine("AGENT ROUTER TEST : Route Message to agentReceiver Synchrone, partner = {0} ", role)
                    //let! replyMessage = agentReceiver.ReceiveMessage(message,channel,role) // Be Carefull: message is the serialized version of the Type
                    let message = agentReceiver.ReceiveMessage(message,role,listTypes,channel) // Be Carefull: message is the serialized version of the Type
                    printfn("trY HERE")
                    channel.Reply(message)                                                                                   // While replyMessage is the message really received from the network 
                    //channel.Reply(replyMessage)
                    System.Console.WriteLine("ALRIGHTTTTT SYNC ! ")//{0}", System.Text.ASCIIEncoding.ASCII.GetString(replyMessage))
                    return! loop()
            }
        in loop()
   
    let agentRouter = Agent.Start(sendAndReceive)
 
    member this.Start() =
        System.Console.WriteLine("AGENT ROUTER STARTING")
        agentReceiver.Start()
        System.Console.WriteLine("AGENT RECEIVER STARTED")
        System.Console.WriteLine("NOMBRE D'AGENT : SENDER = {0} ...",agentMapping.Count)
        for sender in agentMapping do
            sender.Value.Start()
            System.Console.WriteLine("AGENT SENDER STARTED")
               
    member this.SendMessage(message) =
        System.Console.WriteLine("SendMessage METHOD APPELE: dans AgentRouter")
        agentRouter.Post(Message.SendMessage message)
   
    member this.ReceiveMessage(message)=
        System.Console.WriteLine("ReceiveMessage METHOD APPELE: dans AgentRouter")
        let (msg,role,listTypes) = message
        System.Console.WriteLine("Avant ReplyMessage: dans AgentRouter")
        let replyMessage = agentRouter.PostAndReply(fun channel -> Message.ReceiveMessage (msg,role,listTypes,channel))
        System.Console.WriteLine("ReplyMessage recu: dans AgentRouter")
        payloadChoice <- replyMessage.Tail
        replyMessage

    member this.ReceiveMessageAsync(message) = 
        System.Console.WriteLine("ReceiveMessage METHOD APPELE: dans AgentRouter")
        let (msg,role,listTypes) = message
        System.Console.WriteLine("Avant ReplyMessage: dans AgentRouter")
        let replyMessage = agentRouter.PostAndAsyncReply(fun channel -> Message.ReceiveMessageAsync (msg,role,listTypes,channel))
        System.Console.WriteLine("ReplyMessage recu: dans AgentRouter")
        replyMessage        

    member this.ReceiveChoice()=
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
                |true -> let mapAgentSender = configInfos.Partners |> createMapSender <| listRoles
                         let agentReceiver = configInfos.LocalRole.IP |> createReceiver <| configInfos.LocalRole.Port
                         new AgentRouter(mapAgentSender,agentReceiver)



(*
let private createMapAgentSenders (infos:Map<string,string*int>) =
    let mutable mapping = Map.empty<string,AgentSender>
    for partner in infos do
        let info = partner.Value
        let agentSender = new AgentSender(fst(info),snd(info))
        mapping <- mapping.Add(partner.Key,agentSender)
    mapping

let private createAgentReceiver (info:string*int) =
    new AgentReceiver(fst(info),snd(info))

// THIS LIST OF ROLES HAS TO BE GIVEN FROM THE SERVICE API
let private createMapRolePorts (listOfRoles:string list) =
    let rec aux (mapping:Map<string,int>) list port=
        match list with
            |[] -> mapping 
            |hd::tl -> aux (mapping.Add(hd,port)) tl (port+1)
    in aux Map.empty<string,int> listOfRoles 5000


let private createLocalMap (listOfRoles:string list) (localRoleInfos:string*int) (mapRolePorts:Map<string,int>) (localRole:string)=
    let rec aux list (mapping:Map<string,string*int>) =
        match list with
            |[] -> mapping
            |hd::tl -> let mapping = if not(localRole=hd) then 
                                        mapping.Add(hd,("127.0.0.1",mapRolePorts.[hd]))
                                     else
                                        mapping
                       aux tl mapping
    aux listOfRoles Map.empty<string,string*int>



exception TcpInfos of string

// ADD SECURITY BY ADDING CHECKING ON THE NUMBER OF ROLES + RAISE EXCEPTION IF WHAT'S GIVEN IN PARAMETER IS NOT CORRECT
let internal createAgentRouter (local:bool) (partnersInfos:Map<string,string*int>) (localRoleInfos:string*int) listOfRoles (localRole:string) =
    if local then   
        let mapRolePorts = createMapRolePorts listOfRoles
        if (partnersInfos.IsEmpty || (snd(localRoleInfos) = -1)) then
            let partnersInfos = createLocalMap listOfRoles localRoleInfos mapRolePorts localRole
            let ipAddress= fst(localRoleInfos) 
            let localRoleInfos = (ipAddress,mapRolePorts.[localRole])
            
            let agentSenders = createMapAgentSenders partnersInfos
            let agentReceiver = createAgentReceiver localRoleInfos
            new AgentRouter(agentSenders,agentReceiver)
        else
            let agentSenders = createMapAgentSenders partnersInfos
            let agentReceiver = createAgentReceiver localRoleInfos
            new AgentRouter(agentSenders,agentReceiver)
                                 
    else // FOR THE MOMENT THIS SUPPOSES NO MISTAKE FROM THE CODER
        if (partnersInfos.IsEmpty || (snd(localRoleInfos) = -1)) then
            try
                raise (TcpInfos("DISTRIBUTED TCP INFOS MISTAKES"))
            with 
                | TcpInfos str ->   printfn "Tcp Infos Incorrect: %s" str 
                                    // THE NEXT 3 lines of code is for type checking to work
                                    let agentSenders = createMapAgentSenders partnersInfos
                                    let agentReceiver = createAgentReceiver localRoleInfos  
                                    new AgentRouter(agentSenders,agentReceiver) 
        else
            let agentSenders = createMapAgentSenders partnersInfos
            let agentReceiver = createAgentReceiver localRoleInfos
            new AgentRouter(agentSenders,agentReceiver)    *)