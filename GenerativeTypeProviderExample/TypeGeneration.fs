module GenerativeTypeProviderExample.TypeGeneration

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
    
open System.IO


let internal ns = "GenerativeTypeProviderExample.Provided"
let asm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))

let baseType = typeof<obj>

let internal createProvidedType assembly name = 
    ProvidedTypeDefinition(assembly, ns, name, Some baseType, IsErased=false)

let internal createProvidedIncludedType name = 
    ProvidedTypeDefinition(name,Some baseType, IsErased=false)

let internal createMethodType name param=
    ProvidedMethod( name, param, baseType, InvokeCode = (fun args -> <@@ "Hello" + name @@> ))

let internal addTypeToProvidedType (providedType:ProvidedTypeDefinition)=
    asm.AddTypes([providedType])
    providedType

let addMember memberToAdd (providedType:ProvidedTypeDefinition) =
    providedType.AddMember(memberToAdd)
    providedType

let internal addMethod methodType (providedType:ProvidedTypeDefinition) = 
    providedType.AddMember methodType
    providedType    