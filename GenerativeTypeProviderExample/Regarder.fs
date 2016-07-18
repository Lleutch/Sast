module GenerativeTypeProviderExample.Regarder 

open GenerativeTypeProviderExample.CommunicationAgents

let mutable dico = Map.empty<string,AgentRouter>

let ajouter str agent =
    dico <- dico.Add(str,agent)

let startAgentRouter agent =
    dico.Item(agent).Start()

let sendMessage agent message role =
    dico.Item(agent).SendMessage(message,role)

let receiveMessage agent message role =
    dico.Item(agent).ReceiveMessage(message,role) |> ignore
