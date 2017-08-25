module ScribbleGenerativeTypeProvider.Regarder 

open ProviderImplementation.ProvidedTypes
open ScribbleGenerativeTypeProvider.CommunicationAgents
open System.Collections.Concurrent

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
    dico.Item(agent).ReceiveMessage(message,role,listTypes) 

let receiveChoice agent =
    dico.Item(agent).ReceiveChoice()

let mutable cache = Map.empty<string,VarCache>

let initCache name (newCache:VarCache) = 
    cache <- cache.Add(name, newCache)

let printCount1 name = 
    printf "Cache count is: %A" (cache.Item(name))

let printCount name= 
    printf "Cache count is: %i" (cache.Item(name).RuntimeOperation())

let addVars name (keys: string list) (values:int[]) = 
    keys |> List.iteri (fun i key ->  cache.Item(name).Add(key, values.[0]))

(*let printCount1 = 
    printf "Cache count is:"

let printCount = 
    printf "Cache count is:"

let addVars (keys: string list) (values:int[]) = 
    printf "keys values"
*)