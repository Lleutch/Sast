module GenerativeTypeProviderExample.TypeGeneration

// Outside namespaces and modules
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

// ScribbleProvider specific namespaces and modules
open GenerativeTypeProviderExample.DomainModel
open GenerativeTypeProviderExample.CommunicationAgents


(******************* TYPE PROVIDER'S HELPERS *******************)

// CREATING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS
let internal createProvidedType assembly name = 
    ProvidedTypeDefinition(assembly, ns, name, Some baseType)//, IsErased=false)

let internal createProvidedIncludedType name = 
    ProvidedTypeDefinition(name,Some baseType)//, IsErased=false)

let internal createProvidedIncludedTypeChoice typing name =
    ProvidedTypeDefinition(name, typing)// , IsErased=false)

let internal createMethodType name param typing expression =
    ProvidedMethod( name, param, typing, InvokeCode = (fun args -> expression ))

let internal createPropertyType name typing expression =
    ProvidedProperty( name , typing , IsStatic = true, GetterCode = (fun args -> expression ))

let internal createCstor param expression = 
    ProvidedConstructor( parameters = param, InvokeCode = (fun args -> expression ))


// ADDING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS TO THE ASSEMBLY AND AS MEMBERS OF THE TYPE PROVIDER
//let internal addProvidedTypeToAssembly (providedType:ProvidedTypeDefinition)=
//    asm.AddTypes([providedType])
//    providedType

let internal addIncludedTypeToProvidedType nestedTypeToAdd (providedType:ProvidedTypeDefinition) =
    providedType.AddMembers(nestedTypeToAdd)
    providedType

let internal addMethod methodType (providedType:ProvidedTypeDefinition) = 
    providedType.AddMember methodType
    providedType    

let internal addProperty propertyToAdd (providedType:ProvidedTypeDefinition) =
    providedType.AddMember(propertyToAdd)
    providedType

let internal addCstor cstorToAdd (providedType:ProvidedTypeDefinition) =
    providedType.AddMember(cstorToAdd)
    providedType

let internal addMember (memberInfo:#MemberInfo) (providedType:ProvidedTypeDefinition) = 
    providedType.AddMember(memberInfo)
    providedType

let internal addMembers (membersInfo:#MemberInfo list) (providedType:ProvidedTypeDefinition) = 
    providedType.AddMembers(membersInfo)
    providedType


(******************* TYPE PROVIDER'S FUNCTIONS *******************)

let internal findCurrentIndex current (fsmInstance:ScribbleProtocole.Root []) = // gerer les cas
    let mutable inc = 0
    let mutable index = -1 
    for event in fsmInstance do
        match event.CurrentState with
            |n when n=current -> index <- inc
            | _ -> inc <- inc + 1
    index

let internal findNext index (fsmInstance:ScribbleProtocole.Root []) =
    (fsmInstance.[index].NextState)

let internal findNextIndex currentState (fsmInstance:ScribbleProtocole.Root []) =
    let index = findCurrentIndex currentState fsmInstance in
    let next = findNext index fsmInstance in
    findCurrentIndex next fsmInstance

let internal findSameNext nextState  (fsmInstance:ScribbleProtocole.Root [])  =
    let mutable list = []
    let mutable inc = 0
    for event in fsmInstance do
        if event.NextState = nextState then
            list <- inc::list
        inc <- inc+1
    list

let rec alreadySeen (liste:string list) (s:string) =
    match liste with
        | [] -> false
        | hd::tl -> if hd.Equals(s) then
                        true
                    else
                        alreadySeen tl s

let internal findSameCurrent currentState  (fsmInstance:ScribbleProtocole.Root [])  =
    let mutable list = []
    let mutable inc = 0
    for event in fsmInstance do
        if event.CurrentState = currentState then
            list <- inc::list
        inc <- inc+1
    list


// Test this function by changing t with t+1 and see the mistakes happen  -> generate the useless ProvidedTypeDefinition and throw exception cause it
// is not added to the assembly.
let rec findProvidedType (providedList:ProvidedTypeDefinition list) stateValue =
    match providedList with
        |[] -> // Useless case, t is useless but we need this case due to pattern matching exhaustiveness.
                "CodingMistake" |> createProvidedIncludedType 
        |[a] -> let t = ref 0
                if System.Int32.TryParse(a.Name.Replace("State",""),t) && (!t)=stateValue then
                    a
                else 
                    findProvidedType [] stateValue    
        |hd::tl -> let t = ref 0
                   if System.Int32.TryParse(hd.Name.Replace("State",""),t) && (!t)=stateValue then
                       hd
                   else
                       findProvidedType tl stateValue      

                       
let internal makeRoleTypes (fsmInstance:ScribbleProtocole.Root []) = 
    let mutable liste = [fsmInstance.[0].LocalRole]
    let mutable listeType = []
    let ctor = <@@ () @@> |> createCstor []
    let t = fsmInstance.[0].LocalRole 
                                        |> createProvidedIncludedType 
                                        |> addCstor ctor
    let t = t |> addProperty (Expr.NewObject(ctor,[]) |> createPropertyType "instance" t)
    listeType <- t::listeType
    let mutable mapping = Map.empty<_,ProvidedTypeDefinition>.Add(fsmInstance.[0].LocalRole,t)
    for event in fsmInstance do
        if not(alreadySeen liste event.Partner) then
            let ctor = ( <@@ () @@> |> createCstor [])
            let t = event.Partner 
                                    |> createProvidedIncludedType
                                    |> addCstor ctor    
            let t = t |> addProperty (Expr.NewObject(ctor, []) |> createPropertyType "instance" t)                                                                     
            mapping <- mapping.Add(event.Partner,t)
            liste <- event.Partner::liste
            listeType <- t::listeType
    (mapping,listeType)


let internal makeLabelTypes (fsmInstance:ScribbleProtocole.Root []) (providedList: ProvidedTypeDefinition list) = 
    let mutable listeLabelSeen = []
    let mutable listeType = []
    let mutable mapping = Map.empty<_,ProvidedTypeDefinition>
    for event in fsmInstance do
        if (event.Type.Contains("choice") && not(alreadySeen listeLabelSeen event.Label)) then
            
            let choiceType = ("LabelChoice" + string event.CurrentState) |> createProvidedIncludedType
                                                                         |> addCstor ( <@@ () @@> |> createCstor [])
            mapping <- mapping.Add("LabelChoice"+ string event.CurrentState,choiceType)
            listeType <- choiceType::listeType 
            let listIndexChoice = findSameCurrent event.CurrentState fsmInstance
            let rec aux (liste:int list) =
                match liste with
                    |[] -> ()
                    |[aChoice] -> if not(alreadySeen listeLabelSeen fsmInstance.[aChoice].Label) then
                                    let nextType = findProvidedType providedList fsmInstance.[aChoice].NextState
                                    let c = nextType.GetConstructors().[0]
                                    let expression = Expr.NewObject(c, [])  
                                    let name = fsmInstance.[aChoice].Label.Replace("(","").Replace(")","") 
                                    let t = name |> createProvidedIncludedTypeChoice None
                                                 |> addCstor (<@@ () @@> |> createCstor [])
                                    t.SetBaseTypeDelayed( fun() -> choiceType.DeclaringType.GetNestedType("LabelChoice"+ string event.CurrentState))                                   
                                    mapping <- mapping.Add(fsmInstance.[aChoice].Label,t)
                                    listeLabelSeen <- fsmInstance.[aChoice].Label::listeLabelSeen
                                    listeType <- t::listeType     
                    |hd::tl -> if not(alreadySeen listeLabelSeen fsmInstance.[hd].Label) then
                                    let nextType = findProvidedType providedList fsmInstance.[hd].NextState
                                    let c = nextType.GetConstructors().[0]
                                    let expression = Expr.NewObject(c, [])  
                                    let name = fsmInstance.[hd].Label.Replace("(","").Replace(")","") 
                                    let t = name |> createProvidedIncludedTypeChoice None
                                                 |> addCstor (<@@ () @@> |> createCstor [])
                                    t.SetBaseTypeDelayed( fun() -> choiceType.DeclaringType.GetNestedType("LabelChoice"+ string event.CurrentState))
                                    mapping <- mapping.Add(fsmInstance.[hd].Label,t)
                                    listeLabelSeen <- fsmInstance.[hd].Label::listeLabelSeen
                                    listeType <- t::listeType  
                                    aux tl 
            in aux listIndexChoice 
        else if not(alreadySeen listeLabelSeen event.Label) then
            let name = event.Label.Replace("(","").Replace(")","") 
            let t = name |> createProvidedIncludedType
                         |> addCstor (<@@ () @@> |> createCstor [])
            mapping <- mapping.Add(event.Label,t)
            listeLabelSeen <- event.Label::listeLabelSeen
            listeType <- t::listeType
    (mapping,listeType)


let internal makeStateTypeBase (n:int) (s:string) = 
    (s + string n) |> createProvidedIncludedType
                   |> addCstor (<@@ () @@> |> createCstor [])

let internal makeStateType (n:int) = makeStateTypeBase n "State"

let internal serialize (nextType:ProvidedTypeDefinition) = // TODO: DEAL WITH SERIALIZATION AND DESERIALIZATION
    "hey"B

let rec goingThrough (methodName:string) (providedList:ProvidedTypeDefinition list) (aType:ProvidedTypeDefinition) (indexList:int list) 
                     (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) (agentRouter:AgentRouter)=
        match indexList with
        |[] -> // Last state: no next state possible
                aType |> addMethod (<@@ printfn "finish" @@> |> createMethodType methodName [] typeof<unit> ) |> ignore
        |[b] -> let nextType = findProvidedType providedList fsmInstance.[b].NextState
                let c = nextType.GetConstructors().[0]
                let exprState = Expr.NewObject(c, [])
                let expression =
                    match methodName with
                        |"send" ->  <@@ agentRouter.SendMessage((serialize(nextType),fsmInstance.[b].Partner)) 
                                        %%exprState @@>
                        |"receive" -> <@@ let received = agentRouter.ReceiveMessage((serialize(nextType),fsmInstance.[b].Partner))
                                          //let expected = deserialize(nextType)
                                          //raise(Error) 
                                          %%exprState @@>
                        |_ -> <@@ printfn "Error" @@>
                aType 
                    |> addMethod ( expression |> createMethodType methodName [ProvidedParameter("Label",mLabel.[fsmInstance.[b].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[b].Partner])] nextType)
                    |> ignore
        |hd::tl -> let nextType = findProvidedType providedList fsmInstance.[hd].NextState
                   let c = nextType.GetConstructors().[0]
                   let exprState = Expr.NewObject(c, [])
                   let expression = 
                       match methodName with
                           |"send" ->  <@@ agentRouter.SendMessage((serialize(nextType),fsmInstance.[hd].Partner)) 
                                           %%exprState @@>
                           |"receive" -> <@@ let received = agentRouter.ReceiveMessage((serialize(nextType),fsmInstance.[hd].Partner)) 
                                              //let expected = deserialize(nextType)
                                              //raise(Error) 
                                             %%exprState @@>
                           |_ -> <@@ printfn "Error" @@>
                   aType 
                        |> addMethod ( expression |> createMethodType methodName [ProvidedParameter("Label",mLabel.[fsmInstance.[hd].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[hd].Partner])] nextType)
                        |> ignore                
                   goingThrough methodName providedList aType tl mLabel mRole fsmInstance agentRouter


let rec addProperties (providedListStatic:ProvidedTypeDefinition list) (providedList:ProvidedTypeDefinition list) (stateList: int list) 
                      (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) (agentRouter:AgentRouter)=
    let currentState = stateList.Head
    let indexOfState = findCurrentIndex currentState fsmInstance
    let indexList = findSameCurrent currentState fsmInstance 
    let mutable methodName = "finish"
    if indexOfState <> -1 then
        methodName <- fsmInstance.[indexOfState].Type
    match providedList with
        |[] -> ()
        |[aType] -> match methodName with
                        |"send" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance agentRouter
                        |"receive" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance agentRouter
                        |"choice" -> let labelType = mLabel.["LabelChoice"+ string currentState]
                                     let c = labelType.GetConstructors().[0]
                                     let expression = Expr.NewObject(c,[])
                                     aType |> addMethod ( expression |> createMethodType "receive" [] labelType ) |> ignore
                        |"finish" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance agentRouter
                        | _ -> printfn "Not correct"
                    aType |> addProperty (<@@ "essaye Bateau" @@> |> createPropertyType "MyProperty" typeof<string> ) |> ignore
        |hd::tl ->  match methodName with
                        |"send" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance agentRouter
                        |"receive" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance agentRouter
                        |"choice" -> let labelType = mLabel.["LabelChoice"+ string currentState]
                                     let c = labelType.GetConstructors().[0]
                                     let expression = Expr.NewObject(c,[]) 
                                     hd |> addMethod (expression |> createMethodType "receive" [] labelType) |> ignore
                        |"finish" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance agentRouter
                        | _ -> printfn "Not correct"
                    hd |> addProperty (<@@ "Test" @@> |> createPropertyType "MyProperty" typeof<string> ) |> ignore
                    addProperties providedListStatic tl (stateList.Tail) mLabel mRole fsmInstance agentRouter     


let internal contains (aSet:Set<'a>) x = 
    Set.exists ((=) x) aSet

let internal stateSet (fsmInstance:ScribbleProtocole.Root []) =
    let firstState = fsmInstance.[0].CurrentState
    let mutable setSeen = Set.empty
    let mutable counter = 0
    for event in fsmInstance do
        if (not(contains setSeen event.CurrentState) || not(contains setSeen event.NextState)) then
            setSeen <- setSeen.Add(event.CurrentState)
            setSeen <- setSeen.Add(event.NextState)
    (setSeen.Count,setSeen,firstState)

let internal makeRoleList (fsmInstance:ScribbleProtocole.Root []) =
    [yield fsmInstance.[0].LocalRole
     for event in fsmInstance do
        yield event.Partner]
