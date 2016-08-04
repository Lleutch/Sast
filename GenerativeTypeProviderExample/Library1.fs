namespace GenerativeTypeProviderExample

// Outside namespaces and modules
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

// ScribbleProvider specific namespaces and modules
open GenerativeTypeProviderExample.TypeGeneration
open GenerativeTypeProviderExample.DomainModel
open GenerativeTypeProviderExample.CommunicationAgents
open GenerativeTypeProviderExample.Regarder

type Hey()= class end

[<TypeProvider>]
type GenerativeTypeProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let tmpAsm = Assembly.LoadFrom(config.RuntimeAssembly)

    let createType (name:string) (parameters:obj[]) =
        let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
        we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
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

        let local = parameters.[1]  :?> bool
        let partnersInfos = parameters.[2]  :?> Map<string,string*int>
        let localRoleInfos = parameters.[3]  :?> string*int

        
        let agentRouter = createAgentRouter local partnersInfos localRoleInfos listOfRoles protocol.[0].LocalRole
        Regarder.ajouter "agent" agentRouter

        addProperties listTypes listTypes (Set.toList stateSet) (fst tupleLabel) (fst tupleRole) protocol

        let ctor = firstStateType.GetConstructors().[0]                                                               
        let ctorExpr = Expr.NewObject(ctor, [])
        let exprCtor = ctorExpr
        let exprStart = <@@ Regarder.startAgentRouter "agent" @@>
        let expression = Expr.Sequential(exprStart,exprCtor)
        
        name 
                |> createProvidedType tmpAsm
                |> addCstor ( <@@ "hey" + string n @@> |> createCstor [])
                |> addMethod ( expression |> createMethodType "Start" [] firstStateType)
                |> addIncludedTypeToProvidedType list2
                |> addIncludedTypeToProvidedType list1
                |> addIncludedTypeToProvidedType listTypes
                |> addProvidedTypeToAssembly
    
    let basePort = 5000
             
    let providedType = TypeGeneration.createProvidedType tmpAsm "TypeProvider"
    let parameters = [ProvidedStaticParameter("Protocol",typeof<string>);
                      ProvidedStaticParameter("Local",typeof<bool>,parameterDefaultValue = true);
                      ProvidedStaticParameter("PartnersInfos",typeof<Map<string,string*int>>,parameterDefaultValue = Map.empty<string,string*int>);
                      ProvidedStaticParameter("LocalRoleInfos",typeof<string*int>,parameterDefaultValue = ("127.0.0.1",-1));]

    do 
        providedType.DefineStaticParameters(parameters,createType)
        //this.AddNamespace(ns, [providedType])
        this.AddNamespace(ns, [TypeGeneration.addProvidedTypeToAssembly providedType])

[<assembly:TypeProviderAssembly>]
    do()