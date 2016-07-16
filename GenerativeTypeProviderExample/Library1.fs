namespace GenerativeTypeProviderExample

// Outside namespaces and modules
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

// ScribbleProvider specific namespaces and modules
open GenerativeTypeProviderExample.TypeGeneration
open GenerativeTypeProviderExample.DomainModel

[<TypeProvider>]
type GenerativeTypeProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let tmpAsm = Assembly.LoadFrom(config.RuntimeAssembly)

    let createType (name:string) (parameters:obj[]) =
        let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
//we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        let protocol = ScribbleProtocole.Parse(fsm)
        let triple= stateSet protocol
        let n,stateSet,firstState = triple
        let listTypes = (Set.toList stateSet) |> List.map (fun x -> makeStateType x )
        let firstStateType = findProvidedType listTypes firstState
        let tupleLabel = makeLabelTypes protocol listTypes
        let tupleRole = makeRoleTypes protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)
        addProperties listTypes listTypes (Set.toList stateSet) (fst tupleLabel) (fst tupleRole) protocol
        
        let ctorExpr = firstStateType.GetConstructors().[0]                                                               
        let expression = Expr.NewObject(ctorExpr, [])
        name 
                |> createProvidedType tmpAsm
                |> addCstor ( <@@ "hey" + string n @@> |> createCstor [])
                |> addMethod ( expression |> createMethodType "Start" [] firstStateType)
                |> addIncludedTypeToProvidedType list2
                |> addIncludedTypeToProvidedType list1
                |> addIncludedTypeToProvidedType listTypes
                //|> addProvidedTypeToAssembly
            
    let providedType = TypeGeneration.createProvidedType tmpAsm "TypeProvider"
    let parameters = [ProvidedStaticParameter("Protocol",typeof<string>)]

    do 
        providedType.DefineStaticParameters(parameters,createType)
        this.AddNamespace(ns, [providedType])//[TypeGeneration.addProvidedTypeToAssembly providedType])

[<assembly:TypeProviderAssembly>]
    do()