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

    let createType (name:string) (parameters:obj[]) =
        //match parameters with
            //|[||] -> name
          name
            |> TypeGeneration.createProvidedType tmpAsm
            |> TypeGeneration.addCstor (TypeGeneration.createCstor [])
            |> TypeGeneration.addProvidedTypeToAssembly
            //| _ -> failwith "Shouldn't be any parameters"

    let parameters = []
    let providedType = TypeGeneration.createProvidedType tmpAsm "TypeProvider"
    
    do 
        this.AddNamespace(ns, [TypeGeneration.addProvidedTypeToAssembly providedType] )

        providedType.DefineStaticParameters(parameters,createType)
[<assembly:TypeProviderAssembly>]
    do()
