module ScribbleGenerativeTypeProvider.TypeGeneration

// Outside namespaces and modules
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
open System.Text

// ScribbleProvider specific namespaces and modules
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.CommunicationAgents
open ScribbleGenerativeTypeProvider.IO
open ScribbleGenerativeTypeProvider.RefinementTypes
open Common.CFSM
open Common.CommonFSM

(******************* TYPE PROVIDER'S HELPERS *******************)

// CREATING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS
let internal createProvidedType assembly name = 
    ProvidedTypeDefinition(assembly, ns, name, Some baseType, IsErased=false)

let internal createProvidedIncludedType name = 
    ProvidedTypeDefinition(name,Some baseType, IsErased=false)

let internal createProvidedIncludedTypeChoice typing name =
    ProvidedTypeDefinition(name, Some typing , IsErased=false)

let internal createMethodType name param typing expression =
    ProvidedMethod( name, param, typing, InvokeCode = (fun args -> expression ))

let internal createPropertyType name typing expression =
    ProvidedProperty( name , typing , IsStatic = true, GetterCode = (fun args -> expression ))

let internal createCstor param expression = 
    ProvidedConstructor( parameters = param, InvokeCode = (fun args -> expression ))


// ADDING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS TO THE ASSEMBLY AND AS MEMBERS OF THE TYPE PROVIDER
(*let internal addProvidedTypeToAssembly (providedType:ProvidedTypeDefinition)=
    asm.AddTypes([providedType])
    providedType*)

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


(*** ********************************************************* ***)
(******************* ProvidedTypes Generation  *******************)
(*** ********************************************************* ***)

(*** ********************** ***)
(***  Role Types Generation ***)
(*** ********************** ***)
/// Generates a Map between Partners -> Generated Provided Types
let generatePartnersTypes (cfsm:CFSM) =
    let uniquelyDefinedPartners = 
        let (States states) = cfsm.States
        states 
        |> Map.toList |> List.map snd 
        |> List.map 
            (fun sessionType -> 
                match sessionType with
                | Send transition 
                | Receive transition 
                | Request transition 
                | Accept transition -> [transition]

                | Branch (Transitions transitions) 
                | Select (Transitions transitions) -> transitions

                | End -> []
            )
        |> List.concat
        |> List.map (fun transition -> transition.Partner )
        |> Set.ofList

    [ for partner in uniquelyDefinedPartners do
        let (Partner partnerName) = partner
        let ctor = (<@@ () @@> |> createCstor [])
        let providedPartner = 
            let tmpProvidedPartner =
                partnerName
                |> createProvidedIncludedType 
                |> addCstor ctor
            tmpProvidedPartner
            |> addProperty (Expr.NewObject(ctor,[]) |> createPropertyType "instance" tmpProvidedPartner)

        providedPartner.HideObjectMethods <- true
        yield (partner,ProvidedPartner providedPartner)
    ] |> Map.ofList |> GeneratedPartners


(*** ************************ ***)
(***  Label Types Generation  ***)
(*** ************************ ***)
/// Generates a Map between Labels -> Generated Provided Types,
/// When we are in the case of Choice.
// TODO : Change choiceID to have a specific type
let internal generateLabelTypes (transitions:Transitions) (choiceID:int) =
    // TODO : 1) put in a single place for efficiency (this uses reflection) + error handle that
    // TODO : 2) Remove that when going to erased TP
    let assembly = typeof<TypeChoices.Choice1>.Assembly
    let branchInterfaceType = assembly.GetType("ScribbleGenerativeTypeProvider.TypeChoices+Choice" + choiceID.ToString())

    let (Transitions transitions) = transitions
    
    // TODO : Add method implementation
    let generatedLabels =
        [ for transition in transitions do
            let (Label labelName) = transition.Label
            let providedLabel = 
                let labelName = sprintf "Branch%i_%s" choiceID labelName 
                labelName
                |> createProvidedIncludedType
                |> addCstor (<@@ labelName @@> |> createCstor [])

            providedLabel.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
            providedLabel.HideObjectMethods <- true
            providedLabel.AddInterfaceImplementation branchInterfaceType
            
            yield (transition.Label,ProvidedLabel providedLabel)    
        ] |> Map.ofList |> GeneratedLabels
    (InterfaceType branchInterfaceType,generatedLabels)

(*** ************************ ***)
(***  State Types Generation  ***)
(*** ************************ ***)
/// Generates a Map between States -> Generated Provided Types
// TODO : make ctor private in order not to have any States instanciable
let generateStateTypes (cfsm:CFSM) =
    let (States states) = cfsm.States
    let stateTypes = 
        let mutable choiceID = 1
        [ for state in states do
            let stateId = state.Key
            let sessionType = state.Value
            match sessionType with
            | Branch transitions -> 
                let branchName = sprintf "Branch%i" choiceID
                let providedState = 
                    let ctor = (<@@ () @@> |> createCstor [])
                    let tmpProvidedState =
                        branchName
                        |> createProvidedIncludedType 
                        |> addCstor ctor
                    tmpProvidedState               
                let interfaceType,generatedLabels = generateLabelTypes transitions choiceID
                choiceID <- choiceID + 1
                yield 
                    (stateId ,
                     {  interfaceType   = interfaceType
                        branching       = providedState
                        branches        = generatedLabels   
                     } |> ChoiceType)
            
            | End -> 
                let providedState = 
                    let ctor = (<@@ new End() @@> |> createCstor [])
                    let tmpProvidedState =
                        let (StateId id) = stateId
                        sprintf "Channel%i" id
                        |> createProvidedIncludedType 
                        |> addCstor ctor
                    (stateId,EndType tmpProvidedState)               
                yield providedState            
            | _ ->
                let providedState = 
                    let ctor = (<@@ () @@> |> createCstor [])
                    let tmpProvidedState =
                        let (StateId id) = stateId
                        sprintf "Channel%i" id
                        |> createProvidedIncludedType 
                        |> addCstor ctor
                    (stateId,NotChoiceType tmpProvidedState)               
                yield providedState

        ] |> Map.ofList |> GeneratedStates
    stateTypes

(*** ********************************************************* ***)
(*******************   ProvidedTypes Linking   *******************)
(*** ********************************************************* ***)

let getAssertionDoc (Assertion assertion) = 
    if (assertion <> "") then 
        let sb = new System.Text.StringBuilder()
        sb.Append("<summary> Method arguments should satisfy the following constraint:") |> ignore
        sb.Append ("<para>" + assertion.Replace(">", "&gt;").Replace("<","&gt;") + "</para>" ) |>ignore
        sb.Append("</summary>") |>ignore
        sb.ToString()
    else ""   


let internal getAllChoiceLabelString (indexList : int list) (fsmInstance:ScribbleProtocole.Root []) =
    let rec aux list acc =
        match list with
            |[] -> acc
            |hd::tl -> let labelBytes = fsmInstance.[hd].Label 
                       aux tl (labelBytes::acc) 
    in aux indexList []

let getDocForChoice indexList fsmInstance=  
    let sb = new System.Text.StringBuilder()
    sb.Append("<summary> When branching here, you will have to type pattern match on the following types :") |> ignore
    
    (indexList |> getAllChoiceLabelString <| fsmInstance)
    |> List.iter(fun message -> sb.Append ("<para> - " + message + "</para>" ) |> ignore ) 
    |> ignore

    sb.Append("</summary>") |> ignore
    sb.ToString()


// TODO : Find a way to generically handle with Payload Types
// Either via a specific DSL for defining the Types + Assertions
let internal generateProvidedParameters (transition : Transition) (ProvidedPartner providedPartner) =
    let genericBuffer = typeof<Buf<_>>.GetGenericTypeDefinition() 
    let (Partner partner) = transition.Partner
    let (Payloads payloads) = transition.Payloads

    let providedParameters =
        [ for (Payload (PName pName,PType pType)) in payloads do
            let genType = genericBuffer.MakeGenericType(System.Type.GetType(pType))
            yield ProvidedParameter(pName,genType) 
        ]
    ProvidedParameter(partner,providedPartner)::providedParameters



let internal provideMethodLinkingNoBranch (methodLinking : MethodLinkingNoBranch<'a>) = 
    
    let (GeneratedStates generatedStates)       = methodLinking.generatedStates
    let (GeneratedPartners generatedPartners)   = methodLinking.generatedPartners
    let stateID             = methodLinking.stateID
    let transition          = methodLinking.transition
    let invokeCode          = methodLinking.invokeCode
    let invokeCodeParameter = methodLinking.invokeCodeParameter
    let providedState       = generatedStates.Item stateID

    let nextProvidedType = 
        let nextStateID = transition.NextState
        generatedStates.Item nextStateID

    match providedState with
    | EndType _
    | ChoiceType _ -> failwith ""
    | NotChoiceType providedStateType -> 
        let myMethod = 
            let methodName = 
                let (Label label) = transition.Label
                methodLinking.methodName + label

            let parameters =
                let providedPartner = generatedPartners.Item transition.Partner
                generateProvidedParameters transition providedPartner

            let nextProvidedType =
                match nextProvidedType with
                | ChoiceType choice -> choice.branching
                | NotChoiceType providedStateType -> providedStateType
                | EndType endType -> endType

            ProvidedMethod(
                methodName,parameters,nextProvidedType,IsStaticMethod = false,
                InvokeCode = invokeCode invokeCodeParameter
            )                    

        let doc = getAssertionDoc transition.Assertion
        if doc <> "" then  myMethod.AddXmlDoc(doc)                                                                                                                                        
                    
        let providedStateType = 
            providedStateType 
            |> addMethod (myMethod)
            |> NotChoiceType
                    
        (stateID,providedStateType)


let internal provideMethodLinkingBranch (methodLinking : MethodLinkingBranch<'a>) = 
    
    let (GeneratedStates generatedStates)       = methodLinking.generatedStates
    let (GeneratedPartners generatedPartners)   = methodLinking.generatedPartners
    let stateID             = methodLinking.stateID
    let (Transitions transitions) = methodLinking.transitions
    let invokeCode          = methodLinking.invokeCode
    let invokeCodeParameter = methodLinking.invokeCodeParameter
    let providedState       = generatedStates.Item stateID



    match providedState with
    | EndType _
    | NotChoiceType _ -> failwith ""
    | ChoiceType choice ->
        let providedBranching = choice.branching
        let (GeneratedLabels nextLabelTypes) = choice.branches 
        let (InterfaceType interfaceType) = choice.interfaceType 

        let myMethod = 
            let methodName  = "Branch"
            let parameters  = []

            ProvidedMethod(
                methodName,parameters,interfaceType,IsStaticMethod = false,
                InvokeCode = invokeCode invokeCodeParameter
            )                    

        let doc = failwith "Not Implemented Yet!!"
            // getDocForChoice [] fsmInstance
        myMethod.AddXmlDoc(doc)
                    
        let branching = 
            providedBranching
            |> addMethod (myMethod)  

        let branches =
            [ for transition in transitions do
                let (ProvidedLabel currentLabelProvidedType) = nextLabelTypes.Item transition.Label

                let myMethod = 
                    let methodName      = "receive"
                    let parameters      = 
                        let providedPartner = generatedPartners.Item transition.Partner
                        generateProvidedParameters transition providedPartner

                    // TODO : Check the nextState from the cfsm if it is a choice or not.
                    let nextProvidedType = 
                        let nextStateID = transition.NextState
                        match generatedStates.Item nextStateID with
                        | ChoiceType choice              -> choice.branching
                        | NotChoiceType nextProvidedType -> nextProvidedType
                        | EndType endType                -> endType

                    ProvidedMethod(
                        methodName,parameters,nextProvidedType,IsStaticMethod = false,
                        InvokeCode = invokeCode invokeCodeParameter
                    )                    

                let doc = failwith "Not Implemented Yet!!"
                    // getDocForChoice [] fsmInstance
                myMethod.AddXmlDoc(doc)
                    
                let currentLabelProvidedType = 
                    currentLabelProvidedType 
                    |> addMethod (myMethod)
                    |> ProvidedLabel
                yield (transition.Label,currentLabelProvidedType)
            ] |> Map.ofList |> GeneratedLabels
        
        let choice =
            { interfaceType = InterfaceType interfaceType
              branching     = branching
              branches      = branches
            } |> ChoiceType

        (stateID,choice)

let internal provideMethodLinkingEndType (methodLinking : MethodLinkingEnd<'a>) = 
    
    let (GeneratedStates generatedStates)       = methodLinking.generatedStates
    let stateID             = methodLinking.stateID
    let providedState       = generatedStates.Item stateID

    match providedState with
    | ChoiceType _     
    | NotChoiceType _   -> failwith ""
    | EndType providedEndType ->
        let myMethod = 
            let methodName  = "Close"
            let parameters  = []

            ProvidedMethod(
                methodName,parameters,typeof<End>,IsStaticMethod = false,
                InvokeCode = fun _ -> <@@ failwith "Not Implemented Yet!" @@>
            )                    
                    
        let providedEndType = 
            providedEndType
            |> addMethod (myMethod)  
            |> EndType
                    
        (stateID,providedEndType)


let internal generateMethodsBetweenEachStates (cfsm : CFSM) (GeneratedStates generatedStates) =
    let (States states) = cfsm.States
    let generatedPartners = generatePartnersTypes cfsm

    // Updating Generated Provided States by adding methods to them
    // It is done by generating a new GeneratedStates value, no mutation.
    let updatedGeneratedStatesWithMethods =
        [ for state in states do
            let stateID = state.Key
            let sessionType = state.Value      

            match sessionType with
            | Send transition   ->
                let methodLinking =
                    {   transition          = transition
                        generatedStates     = GeneratedStates generatedStates
                        stateID             = stateID
                        generatedPartners   = generatedPartners
                        invokeCode          = fun () _ -> <@@ failwith "Not Implemented Yet!" @@>
                        invokeCodeParameter = ()
                        methodName          = "Send"
                    }                    
                yield provideMethodLinkingNoBranch methodLinking

            | Receive transition ->
                let methodLinking =
                    {   transition          = transition
                        generatedStates     = GeneratedStates generatedStates
                        stateID             = stateID
                        generatedPartners   = generatedPartners
                        invokeCode          = fun () _ -> <@@ failwith "Not Implemented Yet!" @@>
                        invokeCodeParameter = ()
                        methodName          = "Receive"
                    }                    
                yield provideMethodLinkingNoBranch methodLinking
                
            | Request transition ->
                let methodLinking =
                    {   transition          = transition
                        generatedStates     = GeneratedStates generatedStates
                        stateID             = stateID
                        generatedPartners   = generatedPartners
                        invokeCode          = fun () _ -> <@@ failwith "Not Implemented Yet!" @@>
                        invokeCodeParameter = ()
                        methodName          = "Request"
                    }                    
                yield provideMethodLinkingNoBranch methodLinking

            | Accept transition -> 
                let methodLinking =
                    {   transition          = transition
                        generatedStates     = GeneratedStates generatedStates
                        stateID             = stateID
                        generatedPartners   = generatedPartners
                        invokeCode          = fun () _ -> <@@ failwith "Not Implemented Yet!" @@>
                        invokeCodeParameter = ()
                        methodName          = "Accept"
                    }                    
                yield provideMethodLinkingNoBranch methodLinking

            | Select (Transitions transitions) ->
                yield!
                    [ for transition in transitions do
                        let methodLinking =
                            {   transition          = transition
                                generatedStates     = GeneratedStates generatedStates
                                stateID             = stateID
                                generatedPartners   = generatedPartners
                                invokeCode          = fun () _ -> <@@ failwith "Not Implemented Yet!" @@>
                                invokeCodeParameter = ()
                                methodName          = "Send"
                            }                        
                        yield provideMethodLinkingNoBranch methodLinking                    
                    ]

            | Branch transitions -> 
                let methodLinking =
                    {   transitions         = transitions 
                        generatedStates     = GeneratedStates generatedStates
                        stateID             = stateID
                        generatedPartners   = generatedPartners
                        invokeCode          = fun () _ -> <@@ failwith "Not Implemented Yet!" @@>
                        invokeCodeParameter = ()
                    }

                yield provideMethodLinkingBranch methodLinking
            | End ->
                let methodLinking =
                    {   generatedStates     = GeneratedStates generatedStates
                        stateID             = stateID }
                yield provideMethodLinkingEndType methodLinking
        ] |> Map.ofList |> GeneratedStates
    updatedGeneratedStatesWithMethods

        





let internal findCurrentIndex current (fsmInstance:ScribbleProtocole.Root []) = // gerer les cas
    let fsm = Array.toList fsmInstance
    let rec aux (acc:ScribbleProtocole.Root list) count =
        match acc with
            |[] -> -1
            |hd::tl -> if hd.CurrentState = current then   
                          count
                       else
                          aux tl (count+1) 
    aux fsm 0


let rec alreadySeenLabel (liste:(string*int) list) (elem:string*int) =
    match liste with
        | [] -> false
        | (hdS,hdI)::tl ->  if hdS.Equals(elem |> fst) && hdI.Equals(elem |> snd) then
                                true
                            else
                                alreadySeenLabel tl elem

let rec alreadySeenOnlyLabel (liste:(string*int) list) (elem:string) =
    match liste with
        | [] -> false
        | (hdS,_)::tl ->  if hdS.Equals(elem) then
                                true
                            else
                                alreadySeenOnlyLabel tl elem


let rec alreadySeenRole (liste:string list) (elem:string) =
    match liste with
        | [] -> false
        | hd::tl -> if hd.Equals(elem) then
                        true
                    else
                        alreadySeenRole tl elem


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

let internal createProvidedParameters (event : ScribbleProtocole.Root) =
    let generic = typeof<Buf<_>>.GetGenericTypeDefinition() 
    let payload = event.Payload
    let mutable n = 0

    [for param in payload do
        n <- n+1
        if param.VarType.Contains("[]") then
            let nameParam = param.VarType.Replace("[]","")
            let typing = System.Type.GetType(nameParam)
            let arrType = typing.MakeArrayType()
            let genType = generic.MakeGenericType(arrType)
            yield ProvidedParameter((param.VarName),genType) 
        else
            // Currently this Case is throwing an error due to the fact that 
            // The type returned by the scribble API is not an F# type
            // This case should be handled properly
            let genType = generic.MakeGenericType(System.Type.GetType(param.VarType))
            yield ProvidedParameter((param.VarName),genType) 
    ]

(*let internal toList (array:_ []) =
    [for elem in array do
        yield elem ]*)

let internal payloadsToList (payloads: System.Collections.Generic.IEnumerable<ScribbleProtocole.Payload>) =
    [for elem in payloads do
        yield elem.VarType ]


let internal payloadsToListStr (payloads:ScribbleProtocole.Payload []) =
    [for i in 0..(payloads.Length-1) do
        yield payloads.[i].VarName
    
    ]

let internal payloadsToProvidedList (payloads:ScribbleProtocole.Payload []) =
    [for i in 0 ..(payloads.Length-1) do 
            yield ProvidedParameter((payloads.[i].VarName),System.Type.GetType(payloads.[i].VarType))
    ]

                       
let internal makeRoleTypes (fsmInstance:ScribbleProtocole.Root []) = 
    let mutable liste = [fsmInstance.[0].LocalRole]
    let mutable listeType = []
    let ctor = <@@ () @@> |> createCstor []
    let t = fsmInstance.[0].LocalRole 
                                        |> createProvidedIncludedType 
                                        |> addCstor ctor
    let t = t |> addProperty (Expr.NewObject(ctor,[]) |> createPropertyType "instance" t)
    t.HideObjectMethods <- true
    listeType <- t::listeType
    let mutable mapping = Map.empty<_,ProvidedTypeDefinition>.Add(fsmInstance.[0].LocalRole,t)
    for event in fsmInstance do
        if not(alreadySeenRole liste event.Partner) then
            let ctor = ( <@@ () @@> |> createCstor [])
            let t = event.Partner 
                                    |> createProvidedIncludedType
                                    |> addCstor ctor    
            let t = t |> addProperty (Expr.NewObject(ctor, []) |> createPropertyType "instance" t)
            t.HideObjectMethods <- true                                                                     
            mapping <- mapping.Add(event.Partner,t)
            liste <- event.Partner::liste
            listeType <- t::listeType
    (mapping,listeType)


let getAssertionDocObsolete assertion = 
    if (assertion <> "") then 
        let sb = new System.Text.StringBuilder()
        sb.Append("<summary> Method arguments should satisfy the following constraint:") |> ignore
        sb.Append ("<para>" + assertion.Replace(">", "&gt;").Replace("<","&gt;") + "</para>" ) |>ignore
        sb.Append("</summary>") |>ignore
        sb.ToString()
    else ""    

let internal makeLabelTypes (fsmInstance:ScribbleProtocole.Root []) (providedList: ProvidedTypeDefinition list) (mRole:Map<string,ProvidedTypeDefinition>) : Map<string,ProvidedTypeDefinition> * ProvidedTypeDefinition list = 
    let mutable listeLabelSeen = []
    let mutable listeType = []
    let mutable choiceIter = 1
    let mutable mapping = Map.empty<_,ProvidedTypeDefinition>
    for event in fsmInstance do
        if (event.Type.Contains("choice") && not(alreadySeenLabel listeLabelSeen (event.Label,event.CurrentState))) then
            match choiceIter with
            |i when i <= TypeChoices.NUMBER_OF_CHOICES ->   
                let assem = typeof<TypeChoices.Choice1>.Assembly
                let typeCtor = assem.GetType("ScribbleGenerativeTypeProvider.TypeChoices+Choice" + i.ToString())
                //mapping <- mapping.Add("Choice"+ string event.CurrentState,typeCtor)
                //listeType <- typeCtor::listeType 
                choiceIter <- choiceIter + 1
                let listIndexChoice = findSameCurrent event.CurrentState fsmInstance
                let rec aux (liste:int list) =
                    match liste with
                    |[] -> ()
                    |[aChoice] -> 
                        let currEvent = fsmInstance.[aChoice]
                        let name = currEvent.Label.Replace("(","").Replace(")","") 
                        printing "Add types + Ctor = " name
                        let mutable t = name |> createProvidedIncludedType
                                            |> addCstor (<@@ name @@> |> createCstor [])

                        if (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                            t <- mapping.[currEvent.Label] //:?> ProvidedTypeDefinition
                                                                                        
                        let listTypes = createProvidedParameters currEvent
                        let listParam = List.append [ProvidedParameter("Role_State_" + currEvent.NextState.ToString(),mRole.[currEvent.Partner])] listTypes
                        //let listPayload = (toList event.Payload)   
                        let nextType = findProvidedType providedList (currEvent.NextState)
                        let ctor = nextType.GetConstructors().[0]
                        let exprState = Expr.NewObject(ctor, [])
                        let myMethod = 
                            ProvidedMethod("receive",listParam,nextType,
                                           IsStaticMethod = false,
                                           InvokeCode = 
                                            fun args-> 
                                                let buffers = args.Tail.Tail
                                                (*let listPayload = (payloadsToList event.Payload)
                                                let exprDes = deserializeChoice buffers listPayload
                                                Expr.Sequential(exprDes,exprState)//*)                                             

                                                let listPayload = (payloadsToListStr event.Payload)

                                                let assertionString = event.Assertion

                                                let fooName,argsName = 
                                                    if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
                                                        let index = RefinementTypes.dictFunInfos.Count                                                            
                                                        let assertion = RefinementTypes.createFnRule index assertionString
                                                        assertion |> fst |> RefinementTypes.addToDict
                                                        snd assertion 
                                                    else 
                                                        "",[]

                                                let exprDes = deserializeChoice buffers listPayload argsName fooName
                                                Expr.Sequential(exprDes,exprState)
                                          )
                        let doc = getAssertionDocObsolete event.Assertion
                        if doc <> "" then  myMethod.AddXmlDoc(doc)                                                                                                                                        
                        
                        t <- t |> addMethod (myMethod)

                        t.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
                        t.HideObjectMethods <- true
                        t.AddInterfaceImplementation typeCtor
                        
                        if not (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then 
                            mapping <- mapping.Add(currEvent.Label,t)
                            listeType <- (t)::listeType       
                        listeLabelSeen <- (currEvent.Label,currEvent.CurrentState)::listeLabelSeen
                                                                                                     
                    |hd::tl ->  
                        let currEvent = fsmInstance.[hd] 
                        let name = currEvent.Label.Replace("(","").Replace(")","")
                        printing "Add types + Ctor = " name
                        let mutable t = name |> createProvidedIncludedType
                                                |> addCstor (<@@ name @@> |> createCstor [])
//                                                                                                         |> addCstor ([] |> createCstor <|  <@@ () @@>) 
                                                                                    
                        if (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                            t <- mapping.[currEvent.Label] //:?> ProvidedTypeDefinition    
                        //if not(alreadySeenLabel listeLabelSeen (currEvent.Label,currEvent.CurrentState)) then
                            //let name = currEvent.Label.Replace("(","").Replace(")","") 
                                                                                        
                        let listTypes = createProvidedParameters currEvent
                        //let listPayload = (toList currEvent.Payload)   
                        let listParam = List.append [ProvidedParameter("Role_State_" + currEvent.NextState.ToString(),mRole.[currEvent.Partner])] listTypes
                        let nextType = findProvidedType providedList (currEvent.NextState)                                                                                 
                        let ctor = nextType.GetConstructors().[0]
                        let exprState = Expr.NewObject(ctor, [])
                        let myMethod = 
                            ProvidedMethod("receive",listParam,nextType,
                                            IsStaticMethod = false,
                                            InvokeCode = 
                                                fun args-> 
                                                    let buffers = args.Tail.Tail

                                                    let listPayload = (payloadsToListStr event.Payload)

                                                    let assertionString = event.Assertion

                                                    let fooName,argsName = 
                                                        if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
                                                            let index = RefinementTypes.dictFunInfos.Count                                                            
                                                            let assertion = RefinementTypes.createFnRule index assertionString
                                                            assertion |> fst |> RefinementTypes.addToDict
                                                            snd assertion 
                                                        else 
                                                            "",[]

                                                    let exprDes = deserializeChoice buffers listPayload argsName fooName
                                                    Expr.Sequential(exprDes,exprState)
                                          )
                        let doc = getAssertionDocObsolete event.Assertion
                        if doc <> "" then myMethod.AddXmlDoc(doc)     

                        t <- t |> addMethod (myMethod)

                        t.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
                        t.HideObjectMethods <- true
                        t.AddInterfaceImplementation typeCtor
                        if not(alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                            mapping <- mapping.Add(currEvent.Label,t)
                            listeType <- (t)::listeType
                        listeLabelSeen <- (currEvent.Label,currEvent.CurrentState)::listeLabelSeen
                                                                                        
                        aux tl 
                in aux listIndexChoice 
            | _ -> failwith ("number of choices > " + TypeChoices.NUMBER_OF_CHOICES.ToString() + " : This protocol won't be taken in account by this TP. ") 

    (mapping,listeType)


let internal makeStateTypeBase (n:int) (s:string) = 
    let ty = (s + string n) |> createProvidedIncludedType
                            |> addCstor (<@@ s+ string n @@> |> createCstor [])
    ty.HideObjectMethods <- true
    ty

let internal makeStateType (n:int) = makeStateTypeBase n "State"


(*
let rec goingThrough (methodNaming:string) (providedList:ProvidedTypeDefinition list) (aType:ProvidedTypeDefinition) (indexList:int list) 
                     (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
        match indexList with
        |[] -> // Last state: no next state possible
                aType |> addMethod (<@@ printfn "finish" @@> |> createMethodType methodNaming [] typeof<End> ) |> ignore
        |[b] -> let nextType = findProvidedType providedList fsmInstance.[b].NextState

                let methodName = fsmInstance.[b].Type
                let c = nextType.GetConstructors().[0]
                let exprState = Expr.NewObject(c, [])
                let event = fsmInstance.[b]
                let fullName = event.Label
                let labelDelim, payloadDelim, endDelim = getDelims fullName
                printfn "ALLEZZZZZ1111111"
                let decode = new System.Text.UTF8Encoding()
                let message = Array.append (decode.GetBytes(fullName)) (decode.GetBytes(labelDelim.Head))
                let role = event.Partner
                let listTypes = 
                    match methodName with
                        |"send" -> toProvidedList event.Payload
                        |"receive" -> createProvidedParameters event
                        | _ -> []
                let listParam = 
                    match methodName with
                        |"send" | "receive" -> List.append [ProvidedParameter("Role",mRole.[role])] listTypes
                        | _  -> []
                let nameLabel = fullName.Replace("(","").Replace(")","") 
                match methodName with
                    |"send" -> let myMethod = 
                                ProvidedMethod(methodName+nameLabel,listParam,nextType,
                                                IsStaticMethod = false,
                                                InvokeCode = 
                                                    fun args -> 
                                                        let buffers = args.Tail.Tail

                                                        let assertionString = event.Assertion

                                                        let fooName,argsName = 
                                                            if assertionString <> "" then
                                                                let index = RefinementTypes.dictFunInfos.Count                                                            
                                                                let assertion = RefinementTypes.createFnRule index assertionString
                                                                assertion |> fst |> RefinementTypes.addToDict
                                                                snd assertion 
                                                            else 
                                                                "",[]

                                                        let exprAction = 
                                                            <@@ 
                                                                let buf = %(serialize fullName buffers (toList event.Payload) (payloadDelim.Head) (endDelim.Head) (labelDelim.Head) argsName fooName)
                                                                Regarder.sendMessage "agent" (buf:byte[]) role 
                                                            @@>
                                                        let fn eq =
                                                            <@
                                                                if eq then
                                                                    failwith (sprintf "METHOD USED : Send + Label = %A" nameLabel)
                                                                else
                                                                    printing "METHOD USED : Send + Label = " nameLabel 
                                                            @>
                                                        let exprAction = 
                                                            Expr.Sequential(<@@ %(fn false) @@>,exprAction)
                                                        Expr.Sequential(exprAction,exprState) )
                                               
                                              
                               aType 
                                    |> addMethod myMethod
                                    |> ignore
                    |"receive" ->  let myMethod = 
                                    ProvidedMethod( methodName+nameLabel,listParam,nextType,
                                                    IsStaticMethod = false,
                                                    InvokeCode = 
                                                        fun args-> 
                                                            let buffers = args.Tail.Tail
                                                            let listPayload = (toList event.Payload)

                                                            let assertionString = event.Assertion
                                                            
                                                            let fooName,argsName = 
                                                                if assertionString <> "" then
                                                                    let index = RefinementTypes.dictFunInfos.Count                                                            
                                                                    let assertion = RefinementTypes.createFnRule index assertionString
                                                                    assertion |> fst |> RefinementTypes.addToDict
                                                                    snd assertion 
                                                                else 
                                                                    "",[]
                                                                    

                                                            let exprDes = deserialize buffers listPayload [message] role argsName fooName
                                                            let exprDes = 
                                                                Expr.Sequential(<@@ printing "METHOD USED : Receive + Label = " nameLabel @@>,exprDes)
                                                            Expr.Sequential(exprDes,exprState) 
                                                   )
                                   let myMethodAsync =  
                                    ProvidedMethod( (methodName+nameLabel+"Async"),listParam,nextType,
                                                    IsStaticMethod = false,
                                                    InvokeCode = 
                                                        fun args-> 
                                                            let buffers = args.Tail.Tail
                                                            let listPayload = (toList event.Payload)

                                                            let assertionString = event.Assertion

                                                            let fooName,argsName = 
                                                                if assertionString <> "" then
                                                                    let index = RefinementTypes.dictFunInfos.Count                                                            
                                                                    let assertion = RefinementTypes.createFnRule index assertionString
                                                                    assertion |> fst |> RefinementTypes.addToDict
                                                                    snd assertion 
                                                                else 
                                                                    "",[]


                                                            let exprDes = deserializeAsync buffers listPayload [message] role argsName fooName
                                                            Expr.Sequential(exprDes,exprState) 
                                                  )
                                   aType 
                                    |> addMethod myMethod
                                    |> addMethod myMethodAsync
                                    |> ignore                 
                
                    | _ -> failwith " Mistake from the CFSM : Type of the message is neither send nor receive !!!!!!" 

        |hd::tl -> let nextType = findProvidedType providedList fsmInstance.[hd].NextState
                   
                   let methodName = fsmInstance.[hd].Type
                   let c = nextType.GetConstructors().[0]
                   let exprState = Expr.NewObject(c, [])
                   let event = fsmInstance.[hd]
                   let fullName = event.Label
                   let labelDelim, payloadDelim, endDelim = getDelims fullName
                   printfn "ALLLEZZZZ 2222222"
                   let decode = new System.Text.UTF8Encoding()
                   let message = Array.append (decode.GetBytes(fullName)) (decode.GetBytes(labelDelim.Head))
                   let role = event.Partner
                   let listTypes = 
                    match methodName with
                        |"send" -> toProvidedList event.Payload
                        |"receive" -> createProvidedParameters event
                        | _ -> []
                   let listParam = 
                    match methodName with
                        |"send" | "receive" -> List.append [ProvidedParameter("Role",mRole.[role])] listTypes
                        | _  -> []
                   
                   let nameLabel = fullName.Replace("(","").Replace(")","")     
                   match methodName with
                    |"send" -> 
                        let myMethod = 
                            ProvidedMethod( methodName+nameLabel,listParam,nextType,
                                            IsStaticMethod = false,
                                            InvokeCode = 
                                                fun args-> 
                                                    let buffers = args.Tail.Tail

                                                    let assertionString = event.Assertion

                                                    let fooName,argsName = 
                                                        if assertionString <> "" then
                                                            let index = RefinementTypes.dictFunInfos.Count                                                            
                                                            let assertion = RefinementTypes.createFnRule index assertionString
                                                            assertion |> fst |> RefinementTypes.addToDict
                                                            snd assertion 
                                                        else 
                                                            "",[]


                                                    let exprAction = 
                                                        <@@ 
                                                            let buf = %(serialize fullName buffers (toList event.Payload) (payloadDelim.Head) (endDelim.Head) (labelDelim.Head) argsName fooName)
                                                            Regarder.sendMessage "agent" (buf:byte[]) role 
                                                        @@>
                                                    let fn eq =
                                                        <@
                                                            if eq then
                                                                failwith (sprintf "METHOD USED : Send + Label = %A" nameLabel )
                                                            else
                                                                printing "METHOD USED : Send + Label = " nameLabel 
                                                        @>
                                                    let exprAction = 
                                                        Expr.Sequential(<@@ %(fn false) @@>,exprAction)
                                                    Expr.Sequential(exprAction,exprState) 
                                               )
                        aType 
                            |> addMethod myMethod
                            |> ignore
                    |"receive" ->  let myMethod = 
                                    ProvidedMethod( methodName+nameLabel,listParam,nextType,
                                                    IsStaticMethod = false,
                                                    InvokeCode = 
                                                        fun args-> 
                                                            let buffers = args.Tail.Tail
                                                            let listPayload = (toList event.Payload)

                                                            let assertionString = event.Assertion
                                                            let fooName,argsName = 
                                                                if assertionString <> "" then
                                                                    let index = RefinementTypes.dictFunInfos.Count                                                            
                                                                    let assertion = RefinementTypes.createFnRule index assertionString
                                                                    assertion |> fst |> RefinementTypes.addToDict
                                                                    snd assertion 
                                                                else 
                                                                    "",[]
                                                            
                                                            let exprDes = deserialize buffers listPayload [message] role argsName fooName
                                                            let exprDes = 
                                                                Expr.Sequential(<@@ printing "METHOD USED : Receive + Label = " nameLabel @@>,exprDes)
                                                            Expr.Sequential(exprDes,exprState) 
                                                   )
                                   
                                   let myMethodAsync = 
                                    ProvidedMethod( (methodName+nameLabel+"Async"),listParam,nextType,
                                                    IsStaticMethod = false,
                                                    InvokeCode = 
                                                        fun args -> 
                                                            let buffers = args.Tail.Tail
                                                            let listPayload = (toList event.Payload)

                                                            let assertionString = event.Assertion
                                                            let fooName,argsName = 
                                                                if assertionString <> "" then
                                                                    let index = RefinementTypes.dictFunInfos.Count                                                            
                                                                    let assertion = RefinementTypes.createFnRule index assertionString
                                                                    assertion |> fst |> RefinementTypes.addToDict
                                                                    snd assertion 
                                                                else 
                                                                    "",[]

                                                            let exprDes = deserializeAsync buffers listPayload [message] role argsName fooName
                                                            Expr.Sequential(exprDes,exprState) 
                                                  )
                                   aType 
                                    |> addMethod myMethod
                                    |> addMethod myMethodAsync
                                    |> ignore                 
                
                    | _ -> failwith (sprintf " Mistake you have a method named : %s  %d  %d !!!!!!" methodName  (fsmInstance.[hd].NextState) (fsmInstance.[hd].CurrentState) )                    
                   goingThrough methodName providedList aType tl mLabel mRole fsmInstance 

*)
let internal getAllChoiceLabels (indexList : int list) (fsmInstance:ScribbleProtocole.Root []) =
        let rec aux list acc =
            match list with
                |[] -> acc
                |hd::tl -> 
                    printing "getAllChoiceLabels : I run" ""
                    let label = fsmInstance.[hd].Label
                    let labelDelim,_,_ = getDelims label
                    let labelBytes = label |> serLabel <| (labelDelim.Head) 
                    let typing = fsmInstance.[hd].Payload |> List.ofArray
                    aux tl ((labelBytes,typing)::acc) 
        in aux indexList []

let invokeCodeOnSend (args:Expr list) (payload: ScribbleProtocole.Payload [])  (payloadDelim: string List) 
                    (labelDelim : string List)  (endDelim: string List)  (nameLabel:string) exprState role fullName (event:ScribbleProtocole.Root) = 
    let buffers = args.Tail.Tail         
                                                    
    let payloadNames = (payloadsToListStr payload )
    let types = payloadsToList payload
    let assertionString = event.Assertion

    let fooName,argsName = 
        if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
            let index = RefinementTypes.dictFunInfos.Count                                                            
            let assertion = RefinementTypes.createFnRule index assertionString
            assertion |> fst |> RefinementTypes.addToDict
            snd assertion 
        else 
            "",[]

    //let buf = ser buffers
    let exprAction = 
        <@@ let buf = %(serialize fullName buffers payloadNames (payloadDelim.Head) (endDelim.Head) (labelDelim.Head) argsName fooName)
            Regarder.sendMessage "agent" (buf:byte[]) role @@>

    let fn eq =
        <@ if eq then failwith (sprintf "METHOD USED : Send + Label = %A" nameLabel )
            else printing "METHOD USED : Send + Label = " nameLabel @>
    
    let exprAction = 
        Expr.Sequential(<@@ %(fn false) @@>,exprAction)
                                                     
    let cachingSupported = types |> List.filter (fun x -> x <> "Syste,.Int32") 
                                        |> List.length |> (fun x -> x=0) 
    if (cachingSupported=true) then 
        let addToCacheExpr = 
            <@@ let myValues:int [] = (%%(Expr.NewArray(typeof<int>, buffers)):int []) 
                Regarder.addVars "cache" payloadNames myValues  
                @@>
        let exprAction = Expr.Sequential(addToCacheExpr, exprAction)
        let printCacheExpr = <@@ Regarder.printCount "cache" @@>
        let exprAction = Expr.Sequential(printCacheExpr,exprAction)
        Expr.Sequential(exprAction,exprState) 
    else
        Expr.Sequential(exprAction,exprState) 


let invokeCodeOnRequest role exprState= 
    let exprNext = 
        <@@ printing "in request" role  @@>
    let exprState = 
        Expr.Sequential(exprNext,exprState)
    let exprNext = 
        <@@ Regarder.requestConnection "agent" role @@>
    Expr.Sequential(exprNext,exprState)

let invokeCodeOnAccept role exprState= 
    let exprNext = 
        <@@ printing "in accept" role  @@>
    let exprState = 
        Expr.Sequential(exprNext,exprState)
    let exprNext = 
        <@@ Regarder.acceptConnection "agent" role @@>
    Expr.Sequential(exprNext,exprState)

let invokeCodeOnReceive (args:Expr list) (payload: ScribbleProtocole.Payload [])  
                        (nameLabel:string) (message: byte[]) 
                        exprState role assertionString = 
   
    let buffers = args.Tail.Tail
    let listPayload = (payloadsToList payload)

    let fooName,argsName = 
        if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
            let index = RefinementTypes.dictFunInfos.Count                                                            
            let assertion = RefinementTypes.createFnRule index assertionString
            assertion |> fst |> RefinementTypes.addToDict
            snd assertion 
        else 
            "",[]
    let exprDes = deserialize buffers listPayload [message] role argsName fooName
   
    let exprDes = 
        Expr.Sequential(<@@ printing "METHOD USED : Receive + Label = " nameLabel @@>,exprDes)
                                                            
    let cachingSupported = listPayload |> List.filter (fun x -> x <> "Syste,.Int32") 
                                |> List.length |> (fun x -> x=0) 
    if (cachingSupported=true) then 
        let payloadNames = (payloadsToListStr payload)
        
        let addToCacheExpr = 
            <@@ let myValues:Buf<int> [] = (%%(Expr.NewArray(typeof<Buf<int>>, buffers)):Buf<int> []) 
                Regarder.addVarsBufs "cache" payloadNames myValues @@>

        let exprDes = Expr.Sequential(exprDes, addToCacheExpr)                                                            
        let cachePrintExpr = <@@ Regarder.printCount "cache" @@>
        
        let exprDes = Expr.Sequential(exprDes, cachePrintExpr)
        Expr.Sequential(exprDes,exprState) 
    else 
        Expr.Sequential(exprDes,exprState) 

let invokeCodeOnChoice (payload: ScribbleProtocole.Payload []) indexList fsmInstance role = 
    let listPayload = (payloadsToList payload) 
    let listExpectedMessagesAndTypes  = getAllChoiceLabels indexList fsmInstance
    let listExpectedMessages = listExpectedMessagesAndTypes |> List.map fst
    let listExpectedTypes = listExpectedMessagesAndTypes |> List.map snd |> List.map (fun p -> payloadsToList p)
    
    
    <@@ 
        printing "Before Branching : " (listExpectedMessages,listExpectedTypes,listPayload)
        let result = Regarder.receiveMessage "agent" listExpectedMessages role listExpectedTypes 
        let decode = new UTF8Encoding() 
        let labelRead = decode.GetString(result.[0]) 
        let assembly = System.Reflection.Assembly.GetExecutingAssembly() 
        let label = Regarder.getLabelType labelRead 
        let typing = assembly.GetType(label.FullName) 
        System.Activator.CreateInstance(typing,[||]) 
    @@>

let getDocForChoiceObsolete indexList fsmInstance=  
    let sb = new System.Text.StringBuilder()
    sb.Append("<summary> When branching here, you will have to type pattern match on the following types :") |> ignore
    
    (indexList |> getAllChoiceLabelString <| fsmInstance)
    |> List.iter(fun message -> sb.Append ("<para> - " + message + "</para>" ) |> ignore ) 
    |> ignore

    sb.Append("</summary>") |> ignore
    sb.ToString()


let generateMethod aType (methodName:string) listParam nextType (errorMessage:string) 
                   (event: ScribbleProtocole.Root) exprState role = 
    
    let fullName = event.Label
    let nameLabel = fullName.Replace("(","").Replace(")","")

    match methodName with
        |"send" -> 
            let labelDelim, payloadDelim, endDelim = getDelims fullName
            
            let myMethod = 
                ProvidedMethod(methodName+nameLabel, listParam, nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun args-> 
                            invokeCodeOnSend args event.Payload payloadDelim 
                                labelDelim endDelim nameLabel 
                                exprState role fullName event)
                        
            let doc = getAssertionDocObsolete event.Assertion
            if doc <> "" then myMethod.AddXmlDoc(doc)
                        
            aType 
                |> addMethod myMethod
                |> ignore
        |"receive" ->  
            let labelDelim, _, _= getDelims fullName
            let decode = new System.Text.UTF8Encoding()
            let message = Array.append (decode.GetBytes(fullName)) (decode.GetBytes(labelDelim.Head))

            let myMethod = 
                ProvidedMethod(methodName+nameLabel,listParam,nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun args-> 
                            invokeCodeOnReceive args event.Payload nameLabel message 
                                exprState role event.Assertion)
                                   
            let myMethodAsync = 
                ProvidedMethod((methodName+nameLabel+"Async"),listParam,nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun args -> 
                            let buffers = args.Tail.Tail
                            let listPayload = (payloadsToList event.Payload)
                            let assertionString = event.Assertion

                            let fooName,argsName = 
                                if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
                                    let index = RefinementTypes.dictFunInfos.Count                                                            
                                    let assertion = RefinementTypes.createFnRule index assertionString
                                    assertion |> fst |> RefinementTypes.addToDict
                                    snd assertion 
                                else 
                                    "",[]

                            let exprDes = deserializeAsync buffers listPayload [message] role argsName fooName
                            Expr.Sequential(exprDes,exprState))
                            
            let doc = getAssertionDocObsolete event.Assertion
            if doc <> "" then myMethod.AddXmlDoc(doc); myMethodAsync.AddXmlDoc(doc)
                        
            aType 
            |> addMethod myMethod
            |> addMethod myMethodAsync
            |> ignore       
        |"request" ->
            let myMethod = 
                ProvidedMethod(methodName+nameLabel, listParam, nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun _-> 
                            invokeCodeOnRequest role exprState) 
            aType 
                |> addMethod myMethod
                |> ignore
          
        |"accept" ->  
            let myMethod = 
                ProvidedMethod(methodName+nameLabel, listParam, nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun _-> 
                            invokeCodeOnAccept role exprState) 
            aType 
                |> addMethod myMethod
                |> ignore

        | _ -> failwith errorMessage                    

let generateChoice (aType:ProvidedTypeDefinition) (fsmInstance: ScribbleProtocole.Root []) currentState indexList indexOfState  = 
    let assem = typeof<TypeChoices.Choice1>.Assembly
    let labelType = assem.GetType("ScribbleGenerativeTypeProvider.TypeChoices+Choice" + string currentState)
    let event = fsmInstance.[indexOfState]
    let role = event.Partner

    let myMethod = 
        ProvidedMethod( "branch", [],labelType, IsStaticMethod = false, 
            InvokeCode = 
                (fun _  ->  
                    invokeCodeOnChoice event.Payload indexList fsmInstance role))                                                                                         
    let doc = getDocForChoice indexList fsmInstance
    myMethod.AddXmlDoc(doc)
    aType |> addMethod myMethod |> ignore

let generateMethodParams (fsmInstance:ScribbleProtocole.Root []) idx (providedList:ProvidedTypeDefinition list) roleValue= 
    let nextType = findProvidedType providedList fsmInstance.[idx].NextState
    let methodName = fsmInstance.[idx].Type
    let event = fsmInstance.[idx]
    let c = nextType.GetConstructors().[0]
    let exprState = Expr.NewObject(c, [])
                
    let listTypes = 
        match methodName with
            |"send" -> payloadsToProvidedList event.Payload
            |"receive" -> createProvidedParameters event
            | _ -> []
                
    let listParam = 
        match methodName with
            |"send" | "receive" | "accept" | "request" -> List.append [ProvidedParameter("Role", roleValue)] listTypes
            | _  -> []


    let makeReturnTuple = (methodName, listParam, nextType, exprState)
    makeReturnTuple

let rec goingThrough (methodNaming:string) (providedList:ProvidedTypeDefinition list) (aType:ProvidedTypeDefinition) (indexList:int list) 
                     (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
        match indexList with
        |[] -> // Last state: no next state possible
                aType |> addMethod (<@@ printfn "finish" @@> |> createMethodType methodNaming [] typeof<End> ) |> ignore
        |[b] ->  let event = fsmInstance.[b]
                 let role = event.Partner

                 let methodName, listParam, nextType, exprState = generateMethodParams fsmInstance b providedList (mRole.[role])
                 let errorMessage = (sprintf " Mistake you have a method named : %s, that is not expected !" methodName )

                 generateMethod aType methodName listParam nextType errorMessage
                                event exprState role 

        |hd::tl -> let nextState = fsmInstance.[hd].NextState
                   let currentState = fsmInstance.[hd].CurrentState
                   let event = fsmInstance.[hd]
                   let role = event.Partner                 
                   
                   let methodName, listParam, nextType, exprState = generateMethodParams fsmInstance hd providedList (mRole.[role])
                   let errorMessage = (sprintf " Mistake you have a method named : %s  %d  %d !" methodName nextState currentState)
                   
                   generateMethod aType methodName listParam nextType errorMessage
                                  event exprState role 

                   goingThrough methodName providedList aType tl mLabel mRole fsmInstance 


let rec addProperties (providedListStatic:ProvidedTypeDefinition list) (providedList:ProvidedTypeDefinition list) (stateList: int list) 
                      (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
    let currentState = stateList.Head
    let indexOfState = findCurrentIndex currentState fsmInstance
    let indexList = findSameCurrent currentState fsmInstance 
    let mutable methodName = "finish"
    if indexOfState <> -1 then
        methodName <- fsmInstance.[indexOfState].Type
    match providedList with
        |[] -> ()
        |[aType] -> match methodName with
                        |"send" |"receive" |"request" |"accept" 
                            -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance 
                        |"choice" -> generateChoice aType fsmInstance currentState indexList indexOfState
                        |"finish" ->  goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance 
                        | _ -> failwith "The only method name that should be available should be send/receive/choice/finish"
        |hd::tl ->  match methodName with
                        |"send" |"receive" |"request" |"accept" 
                            -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance 
                        |"choice" -> generateChoice hd fsmInstance currentState indexList indexOfState
                        |"finish" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance 
                        | _ -> failwith "The only type of event from the CFSM that should be available is one of the 
                                         following : send/receive/choice"
                    hd |> addProperty (<@@ "Test" @@> |> createPropertyType "MyProperty" typeof<string> ) |> ignore

                    addProperties providedListStatic tl (stateList.Tail) mLabel mRole fsmInstance 


let internal contains (aSet:Set<'a>) x = 
    Set.exists ((=) x) aSet

let internal stateSet (fsmInstance:ScribbleProtocole.Root []) =
    let firstState = fsmInstance.[0].CurrentState
    let mutable setSeen = Set.empty
    for event in fsmInstance do
        if (not(contains setSeen event.CurrentState) || not(contains setSeen event.NextState)) then
            setSeen <- setSeen.Add(event.CurrentState)
            setSeen <- setSeen.Add(event.NextState)
    (setSeen.Count,setSeen,firstState)

let internal makeRoleList (fsmInstance:ScribbleProtocole.Root []) =
    let mutable setSeen = Set.empty
    [yield fsmInstance.[0].LocalRole
     for event in fsmInstance do
        if not(setSeen |> contains <| event.Partner) then
            setSeen <- setSeen.Add(event.Partner)
            yield event.Partner] 



