module GenerativeTypeProviderExample.CommunicationAgents


// Outside namespaces and modules
open System.Net.Sockets
open System.IO
open System.Net

// ScribbleProvider specific namespaces and modules
open GenerativeTypeProviderExample.DomainModel

exception TooManyTriesError of string

type AgentSender(ipAddress,port) =
    
    // FEATURE to add: 5 Tries of 3 seconds and then double the time at each try following Microsoft Standards.
    // FEATURE ADDED
    let connect address p (tcpClient:TcpClient) =
        let rec aux timeout count =
            let tries = 5
            try
                match count with
                    |n when n<tries ->  tcpClient.Connect(IPAddress.Parse(address),p)
                                        if not(tcpClient.Connected) then
                                            async{
                                                do! Async.Sleep(timeout*1000) // Probably better to use return! instead ATTENTIONNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
                                            } |> ignore                                            
                                            aux (timeout*2) (count+1)
                    |_ -> tcpClient.Connect(IPAddress.Parse(address),p)
                          if not(tcpClient.Connected) then
                              raise (TooManyTriesError("You have tried too many times to connect, the partner is not ready to connect with you"))
            with
                | :? System.ArgumentException as ex -> printfn "Argument Exception: %s"  ex.Message
                | :? System.Net.Sockets.SocketException as ex ->  printfn "Socket Exception error code: %d"  ex.ErrorCode
                | :? System.ObjectDisposedException as ex -> printfn "Object Disposed Exception: %s"  ex.Message
                | TooManyTriesError(str) -> printfn "Too Many Tries Error: %s" str
        
        in aux 3 0

    let send (stream:NetworkStream) (actor:Agent<Message>) =
        let rec loop () = async {
            let! msg = actor.Receive()
            match msg with
                |ReceiveMessage (message,channel,role) ->
                    () // Raise an exception Error due to bad coding in the type provider
                    return! loop()      
                |SendMessage (message,role) -> // The serialization is done before
                    do! stream.AsyncWrite(message)
                    return! loop()
            }
        in loop()
 
    let mutable agentSender = None 

    member this.SendMessage(message) =
        match (agentSender:Option<Agent<Message>>) with
            |None -> () // Raise an exception Error due to using this method before the Start method in the type provider 
            |Some sending -> sending.Post(Message.SendMessage message)
    member this.Start() = // Raise an exception due to trying to connect and parsing the IPAddress
        let tcpClientSend = new TcpClient()
        connect ipAddress port tcpClientSend
        let stream = tcpClientSend.GetStream()    
        agentSender <- Some (Agent.Start(send stream))



type AgentReceiver(ipAddress,port) =

    let server = new TcpListener(IPAddress.Parse(ipAddress),port)
    let mutable clientMap = Map.empty
 
    let readAllBytes (s : Stream) =
        let ms = new MemoryStream()
        s.CopyTo(ms)
        ms.ToArray()
 
    let binding (tcpListenerReceive:TcpListener) (actor:Agent<Message>) =
        let rec loop () = async {
            let client = tcpListenerReceive.AcceptTcpClient()
            let stream = client.GetStream()
            // CHANGE BELOW BY READING THE ROLE IN ANOTHER Map<role:string,(IP,PORT)>
            let readRole = readAllBytes stream
            // CHANGE ABOVE BY READING THE ROLE IN ANOTHER Map<role:string,(IP,PORT)>
            clientMap <- clientMap.Add(readRole.ToString(),stream)
            return! loop()
            }
        in loop()
 
    let receive (actor:Agent<Message>) =
        let rec loop () = async {
            let! msg = actor.Receive()
            match msg with
                |SendMessage (message,role)->
                    ()  // Raise an exception Error due to bad coding in the type provider
                    return! loop()      
                |ReceiveMessage (message,channel,role) -> // The UnMarshalling is done outside the Agent Routing Architecture NOT HERE.
                    let stream = clientMap.[role]
                    let read = readAllBytes stream
                    channel.Reply(message)
                    return! loop()
            }
        in loop()
 
    let mutable agentReceiver = None
   
    member this.Start()=
        server.Start()
        Agent.Start(binding server) |> ignore
        agentReceiver <- Some (Agent.Start(receive))

    // To Close the listener to optimize ressource usage and avoid receiving things that do not belong to the protocol.
    // To be done in the finish ProvidedMethod that represent the Ending process in Session Types.
    member this.Stop() =
        for client in clientMap do
            client.Value.Close()
        server.Stop()

    // Be carefull with this function: IF IT'S NONE RAISE AN EXCEPTION
    member this.ReceiveMessage(message) =
        match agentReceiver with
            |Some receive -> receive.PostAndAsyncReply(fun _ -> Message.ReceiveMessage message)
            |None -> async{
                        let label,_,_ = message
                        return label
                     }        





type AgentRouter(agentMap:Map<string,AgentSender>,agentReceiving:AgentReceiver) =
    
    let agentMapping = agentMap
    let agentReceiver = agentReceiving
 
    let sendAndReceive (agentRouter:Agent<Message>) =
        let rec loop () = async{
            let! msg = agentRouter.Receive()
            match msg with
                |SendMessage (message,role) ->
                    let agentSender = agentMapping.[role]
                    agentSender.SendMessage(message,role) // Here, message is the serialized message that needs to be sent
                    return! loop()
                |ReceiveMessage (message,channel,role) -> 
                    let! replyMessage = agentReceiver.ReceiveMessage(message,channel,role) // Be Carefull: message is the serialized version of the Type
                                                                                           // While replyMessage is the message really received from the network 
                    channel.Reply(replyMessage)
                    return! loop()
            }
        in loop()
   
    let agentRouter = Agent.Start(sendAndReceive)
 
    member this.Start() =
        agentReceiver.Start()
        for sender in agentMapping do
            sender.Value.Start()
               
    member this.SendMessage(message) =
        agentRouter.Post(Message.SendMessage message)
   
    member this.ReceiveMessage(message)=
        let (msg,role) = message
        async{
            let! replyMessage = agentRouter.PostAndAsyncReply(fun channel -> Message.ReceiveMessage (msg,channel,role))
            return replyMessage
        }





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


// ADD SECURITY BY ADDING CHECKING ON THE NUMBER OF ROLES + RAISE EXCEPTION IF WHAT'S GIVEN IN PARAMETER IS NOT CORRECT
let internal createAgentRouter (local:bool) (partnersInfos:Map<string,string*int>) (localRoleInfos:string*int) listOfRoles (localRole:string) =
    if local then   
        let mapRolePorts = createMapRolePorts listOfRoles
        let partnersInfos = //if partnersInfos.IsEmpty then 
                                createLocalMap listOfRoles localRoleInfos mapRolePorts localRole
                            //else 
                            //    partnersInfos
        let localRoleInfos = // if (snd(localRoleInfos)<0) then  COMMENTED FOR THE MOMENT BUT HAVE TO FIND A BETTER WAY TO DEAL WITH THIS SITUATION
                                let ipAddress= fst(localRoleInfos) 
                                (ipAddress,mapRolePorts.[localRole])
        let agentSenders = createMapAgentSenders partnersInfos
        let agentReceiver = createAgentReceiver localRoleInfos
        new AgentRouter(agentSenders,agentReceiver)
    else // FOR THE MOMENT THIS SUPPOSES NO MISTAKE FROM THE CODER
        let agentSenders = createMapAgentSenders partnersInfos
        let agentReceiver = createAgentReceiver localRoleInfos
        new AgentRouter(agentSenders,agentReceiver)