namespace GenerativeTypeProviderExample

// Outside namespaces and modules
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
open System.IO
open FSharp.Data
open FSharp.Configuration
// ScribbleProvider specific namespaces and modules
open GenerativeTypeProviderExample.TypeGeneration
open GenerativeTypeProviderExample.DomainModel
open GenerativeTypeProviderExample.CommunicationAgents
open GenerativeTypeProviderExample.Regarder


[<TypeProvider>]
type GenerativeTypeProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let tmpAsm = Assembly.LoadFrom(config.RuntimeAssembly)

    let generateTypes (fsm:string) (name:string) (parameters:obj[]) = 
        let protocol = ScribbleProtocole.Parse(fsm)
        let triple= stateSet protocol
        let n,stateSet,firstState = triple
        let listTypes = (Set.toList stateSet) |> List.map (fun x -> makeStateType x )
        let firstStateType = findProvidedType listTypes firstState
        let tupleRole = makeRoleTypes protocol
        let tupleLabel = makeLabelTypes protocol listTypes (tupleRole |> fst)
        let listOfRoles = makeRoleList protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)

        (*let local = parameters.[0]  :?> bool
        let partnersInfos = parameters.[1]  :?> Map<string,string*int>
        let localRoleInfos = parameters.[2]  :?> string*int *)

        let configFilePath = parameters.[0]  :?> string
        let delimitaters = parameters.[1]  :?> string
        
        let mutable mapping = Map.empty<string,string list* string list * string list>

        let instance = MappingDelimiters.Parse(delimitaters)
        for elem in instance do
            let label = elem.Label
            let delims = elem.Delims
            let delim1 = delims.Delim1 |> Array.toList
            let delim2 = delims.Delim2 |> Array.toList
            let delim3 = delims.Delim3 |> Array.toList
            mapping <- mapping.Add(label,(delim1,delim2,delim3)) 

        (*let serializePath = parameters.[1]  :?> string*string
        let deserializePath = parameters.[2]  :?> string*string
        let deserializeChoicePath = parameters.[3]  :?> string*string*)

        mapping |> DomainModel.modifyMap 

        let naming = __SOURCE_DIRECTORY__ +  "\..\\"  + configFilePath
        DomainModel.config.Load(naming)


        (tupleLabel |> fst) |> Regarder.ajouterLabel
        let agentRouter = (DomainModel.config) |> createRouter <| listOfRoles 
        Regarder.ajouter "agent" agentRouter

        addProperties listTypes listTypes (Set.toList stateSet) (fst tupleLabel) (fst tupleRole) protocol

        let ctor = firstStateType.GetConstructors().[0]                                                               
        let ctorExpr = Expr.NewObject(ctor, [])
        let exprCtor = ctorExpr
        let exprStart = <@@ Regarder.startAgentRouter "agent" @@>
        let expression = Expr.Sequential(exprStart,exprCtor)
            

        let ty = name 
                    |> createProvidedType tmpAsm
                    |> addCstor ( <@@ "hey" + string n @@> |> createCstor [])
                    |> addMethod ( expression |> createMethodType "Start" [] firstStateType)
                    |> addIncludedTypeToProvidedType list2
                    |> addIncludedTypeToProvidedType list1
                    |> addIncludedTypeToProvidedType listTypes
                    //|> addProvidedTypeToAssembly
        let assemblyPath = Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
        let assembly = ProvidedAssembly assemblyPath
        ty.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
        ty.HideObjectMethods <- true
        assembly.AddTypes [ty]
        ty
  

    let createTypeWithFSM (name:string) (parameters:obj[]) =
        let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
        we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        let size = parameters.Length
        generateTypes fsm name parameters.[1..(size-1)]

    let createTypeWithFile (name:string) (parameters:obj[]) =
        
        let file = parameters.[0] :?> string
        let protocol = parameters.[1] :?> string
        let localRole = parameters.[2] :?> string

        let relativePath = __SOURCE_DIRECTORY__ +  "\..\\"  + file
        let code = new System.Text.StringBuilder()
        match (File.Exists(file) , File.Exists(relativePath)) with
            | true , false -> let tmp = File.ReadAllLines(file)
                              for elem in tmp do
                                 code.Append(elem) |> ignore
                               
            | false , true -> let tmp = File.ReadAllLines(relativePath)
                              for elem in tmp do
                                 code.Append(elem) |> ignore
                               
            | true , true | false, false ->  failwith (sprintf "this file path is incorrect: %s" relativePath) 

        let str = code.ToString()
        let replace0 =System.Text.RegularExpressions.Regex.Replace(str,"(\s{2,}|\t+)"," ") 
        let replace2 = System.Text.RegularExpressions.Regex.Replace(replace0,"\"","\\\"")

        let json = ScribbleAPI.Root(code = replace2 , proto = protocol ,role = localRole )
        
        let jsonStr = sprintf """ {"code": "%s", "proto": "%s", "role": "%s" }""" (json.Code) (json.Proto) (json.Role) 
        
        // GET THE FSM FROM THE API
        // http://scribbleapi.azurewebsites.net/
        // http://localhost:8083/
        let fsm = FSharp.Data.Http.RequestString("http://localhost:8083/graph.json", 
                                                    query = ["json",jsonStr] ,
                                                    headers = [ FSharp.Data.HttpRequestHeaders.Accept HttpContentTypes.Json ],
                                                    httpMethod = "GET" )

        let size = parameters.Length
        generateTypes fsm name parameters.[3..(size-1)]
    
    //let basePort = 5000
            
    let providedTypeFSM = TypeGeneration.createProvidedType tmpAsm "TypeProviderFSM"
    let providedTypeFile = TypeGeneration.createProvidedType tmpAsm "TypeProviderFile"
    
    let parametersFSM = [ProvidedStaticParameter("Protocol",typeof<string>);
                         ProvidedStaticParameter("Config",typeof<string>);
                         ProvidedStaticParameter("Delimiter",typeof<string>)]
                        (* ProvidedStaticParameter("SerializeMessagePath",typeof<string*string>);
                         ProvidedStaticParameter("DeserializeMessagePath",typeof<string*string>);
                         ProvidedStaticParameter("DerializeChoicePath",typeof<string*string>)]*)
    
    let parametersFile=  [ProvidedStaticParameter("File Uri",typeof<string>);
                          ProvidedStaticParameter("Global Protocol",typeof<string>);
                          ProvidedStaticParameter("Role",typeof<string>);
                          ProvidedStaticParameter("Config",typeof<string>);
                          ProvidedStaticParameter("Delimiter",typeof<string>)]
                         (* ProvidedStaticParameter("SerializeMessagePath",typeof<string*string>);
                          ProvidedStaticParameter("DeserializeMessagePath",typeof<string*string>);
                          ProvidedStaticParameter("DerializeChoicePath",typeof<string*string>)]*)

    do 
        providedTypeFSM.DefineStaticParameters(parametersFSM,createTypeWithFSM)
        providedTypeFile.DefineStaticParameters(parametersFile,createTypeWithFile)
        
        this.AddNamespace(ns, [providedTypeFSM])
        this.AddNamespace(ns, [providedTypeFile])
[<assembly:TypeProviderAssembly>]
    do()