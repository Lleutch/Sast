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
        match parameters.Length with
            | n when n>0 -> let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
                    //we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
                            let protocol = ScribbleProtocole.Parse(fsm)
                            let couple = stateSet protocol
                            let n = fst(couple)
                            let stateSet = snd(couple)
                            let listTypes = (Set.toList stateSet) |> List.map (fun x -> makeStateType x )
                            let tupleLabel = makeLabelTypes protocol listTypes
                            let tupleRole = makeRoleTypes protocol
                            let list1 = snd(tupleLabel)
                            let list2 = snd(tupleRole)
                            //addProperties listTypes listTypes (Set.toList stateSet) (fst tupleLabel) (fst tupleRole) protocol
        
                            let ctorExpr = listTypes.Head.GetConstructors().[0]                                                               
                            let expression = Expr.NewObject(ctorExpr, [])
                            name 
                                    |> createProvidedType tmpAsm
                                    //|> addMethod ( <@@ l1 @@> |> createMethodType "listeLabel" [] typeof<int> )
                                    //|> addMethod ( <@@ l2 @@> |> createMethodType "listeRole" [] typeof<int> )
                                    |> addCstor ( <@@ "hey" + string n @@> |> createCstor [])
                                    |> addMethod ( expression |> createMethodType "instanciate" [] listTypes.Head )
                                    |> addIncludedTypeToProvidedType list2
                                    |> addIncludedTypeToProvidedType list1
                                    |> addIncludedTypeToProvidedType listTypes
                                    |> addProvidedTypeToAssembly
            | _ -> failwith "un truc pas cool la"
                //|> addMembers list2
                //|> addMembers list1
                //|> addMembers listTypes


        //let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "hey" + string n @@>  )
        //let t = ProvidedTypeDefinition(assemblyRuntime,ns,name,Some typeof<obj>, IsErased = false)


//        let myMethod = ProvidedMethod("instanciate",[], listTypes.Head,InvokeCode = (fun args -> let c = listTypes.Head.GetConstructors().[0]                                                               
  //                                                                                               Expr.NewObject(c, [])))  
    //    t.AddMember(ctor)
     //   t.AddMember(myMethod)
      //  t.AddMembers(list2)
       // t.AddMembers(list1)
       // t.AddMembers(listTypes)
        
        
        
        
        (*
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
          
          *)

    let providedType = TypeGeneration.createProvidedType tmpAsm "TypeProvider"
    let parameters = [ProvidedStaticParameter("Protocol",typeof<string>);
                      ProvidedStaticParameter("UselessParam",typeof<string>)]

    do 
        providedType.DefineStaticParameters(parameters,createType)
        this.AddNamespace(ns, [TypeGeneration.addProvidedTypeToAssembly providedType])

[<assembly:TypeProviderAssembly>]
    do()