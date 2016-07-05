module GenerativeTypeProviderExample.Provider

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

open Microsoft.FSharp.Data
open System
open System.Net.Sockets
open System.IO
open System.Threading


[<TypeProvider>]
type ProviderTest(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    
    let internal ns = "GenerativeTypeProviderExample.Provided"
    let provAsm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
    
    let createType (name:string) (parameters:obj[]) =
        
        
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "hey " + string n + " nombre de receiver" + string numberOfRoles @@> )
        let myMethod = ProvidedMethod("Start",[], firstStateType,InvokeCode = (fun args -> let c = firstStateType.GetConstructors().[0] 
                                                                                           agentRouter.Start()
                                                                                           Expr.NewObject(c, [])))  
        let t = ProvidedTypeDefinition(asm,ns,name,Some typeof<obj>)
        let memberList = listTypes |> List.append list2 |> List.append list1 
        t.AddMembers(memberList)
        t.AddMember(myMethod)
        t.AddMember(ctor)
        t


    // let assembly = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
    // let c = assembly.AddTypes

    let providedType = ProvidedTypeDefinition(asm,ns,"RealProvider",Some typeof<obj>)
    let parameters = [ProvidedStaticParameter("Protocol",typeof<string>);
                      ProvidedStaticParameter("Local",typeof<bool>);
                      ProvidedStaticParameter("Senders",typeof<Map<string,string*int>>,parameterDefaultValue = Map.empty<string,string*int> );
                      ProvidedStaticParameter("Receiver",typeof<string*int>,parameterDefaultValue= ("127.0.0.1",5000) )]

    // ProvidedStaticParameter("SampleIsList", typeof<bool>, parameterDefaultValue = false) INSTEAD OF OPTIONSSSSSS 

    do
        providedType.DefineStaticParameters(parameters,createType)
        this.AddNamespace(ns, [providedType] )
[<assembly:TypeProviderAssembly>]
    do()
