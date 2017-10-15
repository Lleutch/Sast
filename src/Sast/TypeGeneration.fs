namespace ScribbleGenerativeTypeProvider

module TypeGeneration =
    // Outside namespaces and modules
    open Microsoft.FSharp.Quotations
    open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
    open System.Reflection // necessary if we want to use the f# assembly

    // ScribbleProvider specific namespaces and modules
    open ScribbleGenerativeTypeProvider.DomainModel
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
    let internal generateLabelTypes (transitions : Transitions) (choiceID : int) = 
        // TODO : 1) put in a single place for efficiency (this uses reflection) + error handle that
        // TODO : 2) Remove that when going to erased TP
        let assembly = typeof<TypeChoices.Choice1>.Assembly
        let branchInterfaceType = 
            assembly.GetType("ScribbleGenerativeTypeProvider.TypeChoices+Choice" + choiceID.ToString())
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
                  yield (transition.Label, ProvidedLabel providedLabel) 
            ]
            |> Map.ofList
            |> GeneratedLabels
        (InterfaceType branchInterfaceType, generatedLabels)

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

    // TODO : Implement
    let internal getAllChoiceLabelString (Transitions _) (_: 'a []) =
        failwith "Not Implemented Yet!"
//        let rec aux list acc =
//            match list with
//                |[] -> acc
//                |hd::tl -> let labelBytes = fsmInstance.[hd].Label 
//                           aux tl (labelBytes::acc) 
//        in aux indexList []

    // TODO : Implement
    let getDocForChoice indexList fsmInstance =  
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



    let internal provideMethodLinkingNoBranch (methodLinking : MethodLinkingNoBranch) = 
    
        let (GeneratedStates generatedStates)       = methodLinking.generatedStates
        let (GeneratedPartners generatedPartners)   = methodLinking.generatedPartners
        let stateID             = methodLinking.stateID
        let transition          = methodLinking.transition
        let invokeCode          = methodLinking.invokeCode
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

                let (stateInstanciation:Expr) = 
                    let ctor = nextProvidedType.GetConstructors().[0]                    
                    Expr.NewObject(ctor, [])
               
                ProvidedMethod(
                    methodName,parameters,nextProvidedType,IsStaticMethod = false,
                    InvokeCode = invokeCode stateInstanciation
                )                    

            let doc = getAssertionDoc transition.Assertion
            if doc <> "" then  myMethod.AddXmlDoc(doc)                                                                                                                                        
                    
            let providedStateType = 
                providedStateType 
                |> addMethod (myMethod)
                |> NotChoiceType
                    
            (stateID,providedStateType)


    let internal provideMethodLinkingBranch (methodLinking : MethodLinkingBranch) = 
    
        let (GeneratedStates generatedStates)       = methodLinking.generatedStates
        let (GeneratedPartners generatedPartners)   = methodLinking.generatedPartners
        let stateID             = methodLinking.stateID
        let (Transitions transitions) = methodLinking.transitions
        let invokeCode          = methodLinking.invokeCode
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
                    InvokeCode = invokeCode
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


                        let (stateInstanciation:Expr) = 
                            let ctor = nextProvidedType.GetConstructors().[0]                    
                            Expr.NewObject(ctor, [])
               
                        ProvidedMethod(
                            methodName,parameters,nextProvidedType,IsStaticMethod = false,
                            InvokeCode = RuntimeInvokeCode.invokeCodeOnBranchedLabel transition stateInstanciation 
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
                    InvokeCode = fun _ -> <@@ new End() @@>
                )                    
                    
            let providedEndType = 
                providedEndType
                |> addMethod (myMethod)  
                |> EndType
                    
            (stateID,providedEndType)


    let internal generateMethodsBetweenEachStates (cfsm : CFSM) delimiters (GeneratedStates generatedStates) =
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
                            invokeCode          = RuntimeInvokeCode.invokeCodeOnSend delimiters transition
                            methodName          = "Send"
                        }                    
                    yield provideMethodLinkingNoBranch methodLinking

                | Receive transition ->
                    let methodLinking =
                        {   transition          = transition
                            generatedStates     = GeneratedStates generatedStates
                            stateID             = stateID
                            generatedPartners   = generatedPartners
                            invokeCode          = RuntimeInvokeCode.invokeCodeOnReceive delimiters transition
                            methodName          = "Receive"
                        }                    
                    yield provideMethodLinkingNoBranch methodLinking
                
                | Request transition ->
                    let methodLinking =
                        {   transition          = transition
                            generatedStates     = GeneratedStates generatedStates
                            stateID             = stateID
                            generatedPartners   = generatedPartners
                            invokeCode          = RuntimeInvokeCode.invokeCodeOnRequest (transition.Partner)
                            methodName          = "Request"
                        }                    
                    yield provideMethodLinkingNoBranch methodLinking

                | Accept transition -> 
                    let methodLinking =
                        {   transition          = transition
                            generatedStates     = GeneratedStates generatedStates
                            stateID             = stateID
                            generatedPartners   = generatedPartners
                            invokeCode          = RuntimeInvokeCode.invokeCodeOnAccept (transition.Partner)
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
                                    invokeCode          = RuntimeInvokeCode.invokeCodeOnSend delimiters transition
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
                            invokeCode          = RuntimeInvokeCode.invokeCodeOnChoice delimiters transitions
                        }

                    yield provideMethodLinkingBranch methodLinking
                | End ->
                    let methodLinking =
                        {   generatedStates     = GeneratedStates generatedStates
                            stateID             = stateID }
                    yield provideMethodLinkingEndType methodLinking
            ] |> Map.ofList |> GeneratedStates
        
        (updatedGeneratedStatesWithMethods,generatedPartners)

