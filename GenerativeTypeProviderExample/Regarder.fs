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

let receiveMessage agent message role =
    dico.Item(agent).ReceiveMessage(message,role)
    
