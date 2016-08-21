module GenerativeTypeProviderExample.TypeGeneration

// Outside namespaces and modules
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
open System.Threading.Tasks

// ScribbleProvider specific namespaces and modules
open GenerativeTypeProviderExample.DomainModel
open GenerativeTypeProviderExample.CommunicationAgents
open GenerativeTypeProviderExample.IO

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
        | (hdS,hdI)::tl ->  if hdS.Equals(elem) then
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
        let genType = generic.MakeGenericType(System.Type.GetType(param))
        yield ProvidedParameter(("Payload_" + string n),genType)] // returns all the buffer


let internal toProvidedList (array:_ []) =
    [for i in 0..(array.Length-1) do
        yield ProvidedParameter(("Payload_" + string i),System.Type.GetType(array.[i]))]

let internal toList (array:_ []) =
    [for elem in array do
        yield elem ]


                       
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




let internal makeLabelTypes (fsmInstance:ScribbleProtocole.Root []) (providedList: ProvidedTypeDefinition list) (mRole:Map<string,ProvidedTypeDefinition>) = 
    let mutable listeLabelSeen = []
    let mutable listeType = []
    let mutable choiceIter = 1
    let mutable mapping = Map.empty<_,System.Type>
    for event in fsmInstance do
        if (event.Type.Contains("choice") && not(alreadySeenLabel listeLabelSeen (event.Label,event.CurrentState))) then
            match choiceIter with
                |i when i <= TypeChoices.NUMBER_OF_CHOICES ->   let assem = typeof<TypeChoices.Choice1>.Assembly
                                                                let typeCtor = assem.GetType("GenerativeTypeProviderExample.TypeChoices+Choice" + i.ToString())
                                                                mapping <- mapping.Add("Choice"+ string event.CurrentState,typeCtor)
                                                                listeType <- typeCtor::listeType 
                                                                choiceIter <- choiceIter + 1
                                                                let listIndexChoice = findSameCurrent event.CurrentState fsmInstance
                                                                let nextType = findProvidedType providedList (event.NextState)
                                                                let rec aux (liste:int list) =
                                                                    match liste with
                                                                        |[] -> ()
                                                                        |[aChoice] -> let currEvent = fsmInstance.[aChoice]
                                                                                      let name = currEvent.Label.Replace("(","").Replace(")","") 
                                                                                      let mutable t = name |> createProvidedIncludedType
                                                                                                           |> addCstor ([] |> createCstor <|  <@@ () @@>)
                                                                                      if (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                                                                                        t <- mapping.[currEvent.Label] :?> ProvidedTypeDefinition
                                                                                        //t
                                                                                      //if not(alreadySeenLabel listeLabelSeen (currEvent.Label,currEvent.CurrentState)) then
                                                                                        //let name = currEvent.Label.Replace("(","").Replace(")","") 
                                                                                        
                                                                                      let listTypes = createProvidedParameters event
                                                                                      let listParam = List.append [ProvidedParameter("Role_State_" + event.NextState.ToString(),mRole.[event.Partner])] listTypes
                                                                                      let listPayload = (toList event.Payload)   
                                                                                      let myMethod = ProvidedMethod("receive",listParam,nextType,
                                                                                                                      IsStaticMethod = false,
                                                                                                                      InvokeCode = fun args-> let buffers = args.Tail.Tail
                                                                                                                                              let listPayload = (toList event.Payload)
                                                                                                                                              let exprUnit = <@@ () @@>
                                                                                                                                              let exprDes = deserializeChoice buffers listPayload
                                                                                                                                              Expr.Sequential(exprDes,exprUnit) )
                                                                                                                                                
                                                                                      t <- t |> addMethod (myMethod)

                                                                                      t.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
                                                                                      t.HideObjectMethods <- true
                                                                                      t.AddInterfaceImplementation typeCtor
                                                                                      if not (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then 
                                                                                          mapping <- mapping.Add(currEvent.Label,t)
                                                                                          listeType <- (t :> System.Type )::listeType       
                                                                                      listeLabelSeen <- (currEvent.Label,currEvent.CurrentState)::listeLabelSeen
                                                                                                     
                                                                        |hd::tl ->  let currEvent = fsmInstance.[hd] 
                                                                                    let name = currEvent.Label.Replace("(","").Replace(")","")
                                                                                    let mutable t = name |> createProvidedIncludedType
                                                                                                         |> addCstor ([] |> createCstor <|  <@@ () @@>) 
                                                                                    if (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                                                                                        t <- mapping.[currEvent.Label] :?> ProvidedTypeDefinition    
                                                                                    //if not(alreadySeenLabel listeLabelSeen (currEvent.Label,currEvent.CurrentState)) then
                                                                                        //let name = currEvent.Label.Replace("(","").Replace(")","") 
                                                                                        
                                                                                    let listTypes = createProvidedParameters event
                                                                                    let listParam = List.append [ProvidedParameter("Role_State_" + event.NextState.ToString(),mRole.[event.Partner])] listTypes
                                                                                    let listPayload = (toList event.Payload)   
                                                                                    let myMethod = ProvidedMethod("receive",listParam,nextType,
                                                                                                                    IsStaticMethod = false,
                                                                                                                    InvokeCode = fun args-> let buffers = args.Tail.Tail
                                                                                                                                            let listPayload = (toList event.Payload)
                                                                                                                                            let exprUnit = <@@ () @@>
                                                                                                                                            let exprDes = deserializeChoice buffers listPayload
                                                                                                                                            Expr.Sequential(exprDes,exprUnit) )

                                                                                    t <- t |> addMethod (myMethod)

                                                                                    t.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
                                                                                    t.HideObjectMethods <- true
                                                                                    t.AddInterfaceImplementation typeCtor
                                                                                    if not(alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                                                                                        mapping <- mapping.Add(currEvent.Label,t)
                                                                                        listeType <- (t :> System.Type )::listeType
                                                                                    listeLabelSeen <- (currEvent.Label,currEvent.CurrentState)::listeLabelSeen
                                                                                        
                                                                                    aux tl 
                                                                in aux listIndexChoice 
                | _ -> failwith ("number of choices > " + TypeChoices .NUMBER_OF_CHOICES.ToString() + " : This protocol won't be taken in account by this TP. ") 

        else if not(alreadySeenOnlyLabel listeLabelSeen event.Label) then
            let name = event.Label.Replace("(","").Replace(")","") 
            let t = name |> createProvidedIncludedType
                         |> addCstor (<@@ name :> obj @@> |> createCstor [])
            mapping <- mapping.Add(event.Label,t)
            listeLabelSeen <- (event.Label,event.CurrentState)::listeLabelSeen
            listeType <- ( t :> System.Type )::listeType
    (mapping,listeType)


let internal makeStateTypeBase (n:int) (s:string) = 
    let ty = (s + string n) |> createProvidedIncludedType
                            |> addCstor (<@@ s+ string n @@> |> createCstor [])
    ty.HideObjectMethods <- true
    ty

let internal makeStateType (n:int) = makeStateTypeBase n "State"


let rec goingThrough (methodNaming:string) (providedList:ProvidedTypeDefinition list) (aType:ProvidedTypeDefinition) (indexList:int list) 
                     (mLabel:Map<string,System.Type>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
        match indexList with
        |[] -> // Last state: no next state possible
                aType |> addMethod (<@@ printfn "finish" @@> |> createMethodType methodNaming [] typeof<unit> ) |> ignore
        |[b] -> let nextType = findProvidedType providedList fsmInstance.[b].NextState
                let methodName = fsmInstance.[b].Type
                let c = nextType.GetConstructors().[0]
                let exprState = Expr.NewObject(c, [])
                let event = fsmInstance.[b]
                let fullName = event.Label
                let message = serializeLabel fullName
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
                let listPayload = (toList event.Payload)
                let nameLabel = fullName.Replace("(","").Replace(")","") 
                match methodName with
                    |"send" -> let myMethod = ProvidedMethod(methodName+nameLabel,listParam,nextType,
                                                                IsStaticMethod = false,
                                                                InvokeCode = fun args-> let buffers = args.Tail.Tail
                                                                                        let buf = serializeMessage fullName (toList event.Payload) buffers
                                                                                        let exprAction = <@@ Regarder.sendMessage "agent" (%%buf:byte[]) role @@>
                                                                                        Expr.Sequential(exprAction,exprState) )
                               aType 
                                    |> addMethod myMethod
                                    |> ignore
                    |"receive" ->  let myMethod = ProvidedMethod(methodName+nameLabel,listParam,nextType,
                                                                    IsStaticMethod = false,
                                                                    InvokeCode = fun args-> let buffers = args.Tail.Tail
                                                                                            let listPayload = (toList event.Payload)
                                                                                            let exprDes = deserialize buffers listPayload [message] role
                                                                                            Expr.Sequential(exprDes,exprState) )
                                   let myMethodAsync =  ProvidedMethod((methodName+nameLabel+"Async"),listParam,nextType,
                                                                        IsStaticMethod = false,
                                                                        InvokeCode = fun args-> let buffers = args.Tail.Tail
                                                                                                let listPayload = (toList event.Payload)
                                                                                                let exprDes = deserializeAsync buffers listPayload [message] role
                                                                                                Expr.Sequential(exprDes,exprState) )
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
                   let message = serializeLabel(fullName)
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
                    |"send" -> let myMethod = ProvidedMethod(methodName+nameLabel,listParam,nextType,
                                                                IsStaticMethod = false,
                                                                InvokeCode = fun args-> let buffers = args.Tail.Tail
                                                                                        let buf = serializeMessage fullName (toList event.Payload) buffers
                                                                                        let exprAction = <@@ Regarder.sendMessage "agent" (%%buf:byte[]) role @@>
                                                                                        Expr.Sequential(exprAction,exprState) )
                               aType 
                                    |> addMethod myMethod
                                    |> ignore
                    |"receive" ->  let myMethod = ProvidedMethod(methodName+nameLabel,listParam,nextType,
                                                                    IsStaticMethod = false,
                                                                    InvokeCode = fun args-> let buffers = args.Tail.Tail
                                                                                            let listPayload = (toList event.Payload)
                                                                                            let exprDes = deserialize buffers listPayload [message] role
                                                                                            Expr.Sequential(exprDes,exprState) )
                                   let myMethodAsync =  ProvidedMethod((methodName+nameLabel+"Async"),listParam,nextType,
                                                                        IsStaticMethod = false,
                                                                        InvokeCode = fun args-> let buffers = args.Tail.Tail
                                                                                                let listPayload = (toList event.Payload)
                                                                                                let exprDes = deserializeAsync buffers listPayload [message] role
                                                                                                Expr.Sequential(exprDes,exprState) )
                                   aType 
                                    |> addMethod myMethod
                                    |> addMethod myMethodAsync
                                    |> ignore                 
                
                    | _ -> failwith (sprintf " Mistake you have a method named : %s !!!!!!" methodName  )                    
                   goingThrough methodName providedList aType tl mLabel mRole fsmInstance 


let internal getAllChoiceLabels (indexList : int list) (fsmInstance:ScribbleProtocole.Root []) =
    let rec aux list acc =
        match list with
            |[] -> acc
            |hd::tl -> let labelBytes = fsmInstance.[hd].Label |> serializeLabel
                       aux tl (labelBytes::acc) 
    in aux indexList []


let internal getAllChoiceLabelString (indexList : int list) (fsmInstance:ScribbleProtocole.Root []) =
    let rec aux list acc =
        match list with
            |[] -> acc
            |hd::tl -> let labelBytes = fsmInstance.[hd].Label 
                       aux tl (labelBytes::acc) 
    in aux indexList []


let rec addProperties (providedListStatic:ProvidedTypeDefinition list) (providedList:ProvidedTypeDefinition list) (stateList: int list) 
                      (mLabel:Map<string,System.Type>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
    let currentState = stateList.Head
    let indexOfState = findCurrentIndex currentState fsmInstance
    let indexList = findSameCurrent currentState fsmInstance 
    let mutable choiceIter = 1
    let mutable methodName = "finish"
    if indexOfState <> -1 then
        methodName <- fsmInstance.[indexOfState].Type
    match providedList with
        |[] -> ()
        |[aType] -> match methodName with
                        |"send" ->  goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance
                        |"receive" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance 
                        |"choice" -> let labelType = mLabel.["Choice" + string currentState]
                                     //let c = labelType.GetConstructors().[0]
                                     //let exprLabel = Expr.NewObject(c,[])
                                     let listExpectedMessages = getAllChoiceLabels indexList fsmInstance
                                     let event = fsmInstance.[indexOfState]
                                     let role = event.Partner
                                     let myMethod = ProvidedMethod("branch",[],labelType,
                                                                                    IsStaticMethod = false,
                                                                                    InvokeCode = fun args-> let listPayload = (toList event.Payload)
                                                                                                            <@@ let result = Regarder.receiveMessage "agent" listExpectedMessages role listPayload 
                                                                                                                let unicode = new System.Text.UnicodeEncoding() 
                                                                                                                let labelRead = unicode.GetString(result.[0]) 
                                                                                                                let label = Regarder.getLabelType labelRead
                                                                                                                let ctor = label.GetConstructors().[0]
                                                                                                                ctor.Invoke([||]) @@> )                                                                                                            
                                     let sb = new System.Text.StringBuilder()
                                     sb.Append("<summary> When branching here, you will have to type pattern match on the following types :") |> ignore
                                     (indexList |> getAllChoiceLabelString <| fsmInstance)
                                                                |> List.iter(fun message -> sb.Append ("<para> - " + message + "</para>" ) |> ignore ) |> ignore
                                     sb.Append("</summary>") |> ignore
                                     let doc = sb.ToString()
                                     myMethod.AddXmlDoc(doc)
                                     aType |> addMethod myMethod |> ignore
                        |"finish" ->  goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance 
                        | _ -> failwith "The only method name that should be available should be send/receive/choice/finish"
        |hd::tl ->  match methodName with
                        |"send" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance 
                        |"receive" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance 
                        |"choice" -> let labelType = mLabel.["Choice"+ string currentState]
                                     //let c = labelType.GetConstructors().[0]
                                     //let exprLabel = Expr.NewObject(c,[])
                                     let listExpectedMessages = getAllChoiceLabels indexList fsmInstance
                                     let event = fsmInstance.[indexOfState]
                                     let role = event.Partner
                                     let myMethod = ProvidedMethod("branch",[],labelType,
                                                                                    IsStaticMethod = false,
                                                                                    InvokeCode = fun args-> let listPayload = (toList event.Payload)
                                                                                                            <@@ let result = Regarder.receiveMessage "agent" listExpectedMessages role listPayload 
                                                                                                                let unicode = new System.Text.UnicodeEncoding() 
                                                                                                                let labelRead = unicode.GetString(result.[0]) 
                                                                                                                let label = Regarder.getLabelType labelRead
                                                                                                                let ctor = label.GetConstructors().[0]
                                                                                                                ctor.Invoke([||]) @@> )                                                                                       
                                     let sb = new System.Text.StringBuilder()
                                     sb.Append("<summary> When branching here, you will have to type pattern match on the following types :") |> ignore
                                     (indexList |> getAllChoiceLabelString <| fsmInstance)
                                                                |> List.iter(fun message -> sb.Append ("<para> - " + message + "</para>" ) |> ignore ) |> ignore
                                     sb.Append("</summary>") |> ignore
                                     let doc = sb.ToString()
                                     myMethod.AddXmlDoc(doc)
                                     hd |> addMethod myMethod |> ignore
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
    let mutable counter = 0
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