namespace GenerativeTypeProviderExample

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

open Microsoft.FSharp.Data
open System
open System.Net.Sockets
open System.IO
open System.Threading
open GenerativeTypeProviderExample.TypeGeneration

[<TypeProvider>]
type GenerativeTypeProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
   
    let tmpAsm = Assembly.LoadFrom(config.RuntimeAssembly)
    
    let providedType = TypeGeneration.createProvidedType tmpAsm "TypeProvider"

    let createType (name:string) (parameters:obj[]) =
        match parameters with
            |[||] -> name
                        |> TypeGeneration.createProvidedType tmpAsm
                        |> TypeGeneration.addMember (TypeGeneration.createMethodType "You" [])
                        |> TypeGeneration.addMember (TypeGeneration.createProvidedIncludedType "NestedType" |> (TypeGeneration.createMethodType "NestedMethod" [] |> TypeGeneration.addMember ))
                        |> TypeGeneration.addTypeToProvidedType
            | _ -> failwith "Shouldn't be any parameters"

    let parameters = []

    do 
        providedType.DefineStaticParameters(parameters,createType)
        this.AddNamespace(ns, [TypeGeneration.addTypeToProvidedType providedType] )
[<assembly:TypeProviderAssembly>]
    do()
