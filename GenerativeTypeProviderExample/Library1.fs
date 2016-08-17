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
        let tupleLabel = makeLabelTypes protocol listTypes
        let tupleRole = makeRoleTypes protocol
        let listOfRoles = makeRoleList protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)

        (*let local = parameters.[0]  :?> bool
        let partnersInfos = parameters.[1]  :?> Map<string,string*int>
        let localRoleInfos = parameters.[2]  :?> string*int *)

        let configFilePath = parameters.[0]  :?> string

        let naming = __SOURCE_DIRECTORY__ + "\\" + configFilePath
        DomainModel.config.Load(naming)


        (tupleLabel |> fst) |> Regarder.ajouterLabel
        let agentRouter = (DomainModel.config) |> createRouter <| listOfRoles 
        //let agentRouter = createAgentRouter local partnersInfos localRoleInfos listOfRoles protocol.[0].LocalRole
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
                    |> addProvidedTypeToAssembly
        ty.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
        ty.HideObjectMethods <- true
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

        let code = new System.Text.StringBuilder()
        match File.Exists(file) with
            | true -> let tmp = File.ReadAllLines(file)
                      for elem in tmp do
                        code.Append(elem) |> ignore
            | false -> failwith "this file path is incorrect!!"
        let json = ScribbleAPI.Root(code = code.ToString(), proto = protocol ,role = localRole )
        
        let jsonStr = sprintf """ {"code": "%s", "proto": "%s", "role": "%s" }""" (json.Code) (json.Proto) (json.Role) 

        (*let textJson = """{"code":"module demo;type <dotnet> \"System.Int32\" from \"s\" as Integer;global protocol Fibonacci(role A, role B) { rec Fib { choice at A { fibonacci(Integer,Integer) from A to B; fibonacci(Integer) \nfrom B to A; continue Fib;}or{end() from A to B;}}}",
                           "proto":"demo.Fibonacci",
                           "role":"A"} """ *)
        
        // GET THE FSM FROM THE API
        // http://scribbleapi.azurewebsites.net/
        // http://localhost:8083/
        let fsm = FSharp.Data.Http.RequestString("http://apiscribble.azurewebsites.net/graph.json", 
                                                    query = ["json",jsonStr] ,
                                                    headers = [ FSharp.Data.HttpRequestHeaders.Accept HttpContentTypes.Json ],
                                                    httpMethod = "GET" )

        let size = parameters.Length
        generateTypes fsm name parameters.[3..(size-1)]
    
    //let basePort = 5000
            
    let providedTypeFSM = TypeGeneration.createProvidedType tmpAsm "TypeProviderFSM"
    let providedTypeFile = TypeGeneration.createProvidedType tmpAsm "TypeProviderFile"
    
    let parametersFSM = [ProvidedStaticParameter("Protocol",typeof<string>);
                         ProvidedStaticParameter("Config",typeof<string>);]
                      (*ProvidedStaticParameter("Local",typeof<bool>,parameterDefaultValue = true);
                      ProvidedStaticParameter("PartnersInfos",typeof<Map<string,string*int>>,parameterDefaultValue = Map.empty<string,string*int>);
                      ProvidedStaticParameter("LocalRoleInfos",typeof<string*int>,parameterDefaultValue = ("127.0.0.1",-1));]*)
    
    let parametersFile= [ ProvidedStaticParameter("File Uri",typeof<string>);
                          ProvidedStaticParameter("Global Protocol",typeof<string>);
                          ProvidedStaticParameter("Role",typeof<string>);
                          ProvidedStaticParameter("Config",typeof<string>);]

    do 
        providedTypeFSM.DefineStaticParameters(parametersFSM,createTypeWithFSM)
        providedTypeFile.DefineStaticParameters(parametersFile,createTypeWithFile)
        
        //this.AddNamespace(ns, [providedType])
        
        this.AddNamespace(ns, [addProvidedTypeToAssembly providedTypeFSM])
        this.AddNamespace(ns, [addProvidedTypeToAssembly providedTypeFile])
[<assembly:TypeProviderAssembly>]
    do()