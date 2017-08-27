module ScribbleGenerativeTypeProvider.Regarder 

open ProviderImplementation.ProvidedTypes
open ScribbleGenerativeTypeProvider.CommunicationAgents

let mutable dico = Map.empty<string,AgentRouter>
let mutable changed = false
let mutable mLabel = Map.empty<string,ProvidedTypeDefinition>

let ajouter str agent =
    if not(changed) then
        dico <- dico.Add(str,agent)
        changed <- true

let ajouterLabel mapping =
    mLabel <- mapping

let getLabelType (labelRead:string) =
    printfn "getLabelType : %A" (mLabel,labelRead) 
     
    mLabel.[labelRead]

let startAgentRouter agent =
    dico.Item(agent).Start()

let sendMessage agent message role =
    dico.Item(agent).SendMessage(message,role)

let receiveMessageAsync agent message role listTypes =
    dico.Item(agent).ReceiveMessageAsync(message,role,listTypes) 

let receiveMessage agent message role listTypes =
    let messageAndTypes = List.zip message listTypes
    dico.Item(agent).ReceiveMessage(messageAndTypes,role) 

let receiveChoice agent =
    dico.Item(agent).ReceiveChoice()

