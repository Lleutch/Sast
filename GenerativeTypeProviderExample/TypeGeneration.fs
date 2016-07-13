module GenerativeTypeProviderExample.TypeGeneration

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
    
open System.IO


let internal ns = "GenerativeTypeProviderExample.Provided"
let asm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))

let baseType = typeof<obj>


// CREATING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS
let internal createProvidedType assembly name = 
    ProvidedTypeDefinition(assembly, ns, name, Some baseType, IsErased=false)

let internal createProvidedIncludedType name = 
    ProvidedTypeDefinition(name,Some baseType, IsErased=false)

let internal createMethodType name param =
    ProvidedMethod( name, param, typeof<string>, InvokeCode = (fun args -> <@@ name @@> ))

let internal createPropertyType name =
    ProvidedProperty( name , typeof<string>, IsStatic = true, GetterCode = (fun args -> <@@ name @@>) )

let internal createCstor param = 
    ProvidedConstructor( parameters = param, InvokeCode = (fun args -> <@@ () @@>) )


// ADDING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS TO THE ASSEMBLY AND AS MEMBERS OF THE TYPE PROVIDER
let internal addProvidedTypeToAssembly (providedType:ProvidedTypeDefinition)=
    asm.AddTypes([providedType])
    providedType

let internal addIncludedTypeToProvidedType nestedTypeToAdd (providedType:ProvidedTypeDefinition) =
    providedType.AddMembers([nestedTypeToAdd])
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