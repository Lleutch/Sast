module GenerativeTypeProviderExample.Regarder 

open GenerativeTypeProviderExample.CommunicationAgents

let mutable dico = Map.empty<string,AgentRouter>
let mutable changed = false

let ajouter str agent =
    if not(changed) then
        dico <- dico.Add(str,agent)
        changed <- true

let startAgentRouter agent =
    dico.Item(agent).Start()

let sendMessage agent message role =
    dico.Item(agent).SendMessage(message,role)

let receiveMessageAsync agent message role listTypes =
    dico.Item(agent).ReceiveMessageAsync(message,role,listTypes) 

let receiveMessage agent message role listTypes =
    dico.Item(agent).ReceiveMessage(message,role,listTypes) 
    //"agent" args message role (toList event.Payload)