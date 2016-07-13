namespace GenerativeTypeProviderExample

// Opening necessary Namespaces and Modules
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System
open System.Reflection // necessary if we want to use the f# assembly
open GenerativeTypeProviderExample.TypeGeneration

[<TypeProvider>]
type GenerativeTypeProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let tmpAsm = Assembly.LoadFrom(config.RuntimeAssembly)

    let createType (name:string) (parameters:obj[]) =
        let possibleType = "State"
                                    |> TypeGeneration.createProvidedIncludedType


        let autrePossibleType = "Autre"
                                    |> TypeGeneration.createProvidedIncludedType

        name
                |> createProvidedType tmpAsm
                |> addCstor (createCstor [])         
                |> addCstor (createCstor [ProvidedParameter("MyAttribute",typeof<string>)])
                |> addMethod (createMethodType "MyMethod" [])
                |> addProperty (createPropertyType "MyProperty")
                |> TypeGeneration.addIncludedTypeToProvidedType possibleType
                |> TypeGeneration.addIncludedTypeToProvidedType autrePossibleType
                |> TypeGeneration.addProvidedTypeToAssembly
          


    let providedType = TypeGeneration.createProvidedType tmpAsm "TypeProvider"
    let parameters = [ProvidedStaticParameter("UselessParam",typeof<string>,parameterDefaultValue = "")]

    do 
        providedType.DefineStaticParameters(parameters,createType)
        this.AddNamespace(ns, [TypeGeneration.addProvidedTypeToAssembly providedType])

[<assembly:TypeProviderAssembly>]
    do()