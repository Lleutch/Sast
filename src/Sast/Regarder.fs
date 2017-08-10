module ScribbleGenerativeTypeProvider.Regarder 

open ScribbleGenerativeTypeProvider.CommunicationAgents

let mutable dico = Map.empty<string,AgentRouter>
let mutable changed = false
let mutable mLabel = Map.empty<string,System.Type>

let ajouter str agent =
    if not(changed) then
        dico <- dico.Add(str,agent)
        changed <- true

let ajouterLabel mapping =
    mLabel <- mapping

let getLabelType (labelRead:string) =
    mLabel.[labelRead]

let startAgentRouter agent =
    dico.Item(agent).Start()

let sendMessage agent message role =
    dico.Item(agent).SendMessage(message,role)

let receiveMessageAsync agent message role listTypes =
    dico.Item(agent).ReceiveMessageAsync(message,role,listTypes) 

let receiveMessage agent message role listTypes =
    dico.Item(agent).ReceiveMessage(message,role,listTypes) 

let receiveChoice agent =
    dico.Item(agent).ReceiveChoice()