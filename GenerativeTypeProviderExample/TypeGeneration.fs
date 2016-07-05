module GenerativeTypeProviderExample.TypeGeneration

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

open Microsoft.FSharp.Data
open System
open System.Net.Sockets
open System.IO
open System.Threading


let internal ns = "GenerativeTypeProviderExample.Provided"
let asm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))


let internal createProvidedType () =
    