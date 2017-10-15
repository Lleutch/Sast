namespace ScribbleGenerativeTypeProvider

// TODO : Update with proper serialization + deserialization strategy.
// TODO : Also => proper assertion + communication to agents
// TODO : Really it's not readable at all
module RuntimeInvokeCode = 
    open ScribbleGenerativeTypeProvider.DomainModel
    open ScribbleGenerativeTypeProvider.IO
    open ScribbleGenerativeTypeProvider.RefinementTypes
    open Microsoft.FSharp.Quotations
    open Common.CFSM
    open Common.CommonFSM
    open System.Text


    let invokeCodeOnSend (delimiters:Delimiters) (transition:Transition) (stateInstanciation:Expr) (args:Expr list) = 

        let payloadsData    = args.Tail.Tail                                           
        let payloadsTypes   = 
            let (Payloads payloads ) = transition.Payloads
            [ for (Payload (_, ( PType pType))) in payloads -> pType ]
        let (Assertion assertion)   = transition.Assertion
        let (Partner partner)       = transition.Partner
        let (Label label)           = transition.Label 

        let fooName,argsName = 
            if ((assertion <> "fun expression -> expression") && (assertion <> ""))  then
                let index = RefinementTypes.dictFunInfos.Count                                                            
                let assertion = RefinementTypes.createFnRule index assertion
                assertion |> fst |> RefinementTypes.addToDict
                snd assertion 
            else 
                "",[]

        let exprAction = 
            <@@ let buf = %(serialize label payloadsData payloadsTypes (delimiters.payloadDelimiter) (delimiters.endDelimiter) (delimiters.labelDelimiter) argsName fooName)
                Regarder.sendMessage "agent" (buf:byte[]) partner  @@>

        let cachingSupported = 
            payloadsTypes 
            |> List.filter (fun x -> x <> "System.Int32") 
            |> List.length 
            |> (fun x -> x=0) 

        if cachingSupported then 
            let addToCacheExpr = 
                <@@ let myValues:int [] = (%%(Expr.NewArray(typeof<int>, payloadsData)):int []) 
                    Regarder.addVars "cache" payloadsTypes myValues  
                    @@>
            let exprAction = Expr.Sequential(addToCacheExpr, exprAction)
            let printCacheExpr = <@@ Regarder.printCount "cache" @@>
            let exprAction = Expr.Sequential(printCacheExpr,exprAction)
            Expr.Sequential(exprAction,stateInstanciation) 
        else
            Expr.Sequential(exprAction,stateInstanciation) 


    let invokeCodeOnRequest (Partner partner) (stateInstanciation:Expr) (_:Expr list) = 
        let exprNext = 
            <@@ printing "in request" partner  @@>
        let exprState = 
            Expr.Sequential(exprNext,stateInstanciation)
        let exprNext = 
            <@@ Regarder.requestConnection "agent" partner @@>
        Expr.Sequential(exprNext,exprState)


    let invokeCodeOnAccept (Partner partner) (stateInstanciation:Expr) (_:Expr list) = 
        let exprNext = 
            <@@ printing "in accept" partner  @@>
        let exprState = 
            Expr.Sequential(exprNext,stateInstanciation)
        let exprNext = 
            <@@ Regarder.acceptConnection "agent" partner @@>
        Expr.Sequential(exprNext,exprState)

    let invokeCodeOnReceive (delimiters:Delimiters) (transition:Transition) (stateInstanciation:Expr) (args:Expr list) = 
   
        let payloadsData    = args.Tail.Tail
        let payloadsTypes   = 
            let (Payloads payloads ) = transition.Payloads
            [ for (Payload (_, ( PType pType))) in payloads -> pType ]
        let (Assertion assertion)   = transition.Assertion
        let (Partner partner)       = transition.Partner
        let (Label label)           = transition.Label 

        let fooName,argsName = 
            if ((assertion <> "fun expression -> expression") && (assertion <> ""))  then
                let index = RefinementTypes.dictFunInfos.Count                                                            
                let assertion = RefinementTypes.createFnRule index assertion
                assertion |> fst |> RefinementTypes.addToDict
                snd assertion 
            else 
                "",[]

        let expectedLabel = serLabel label (delimiters.labelDelimiter)

        let exprDes = deserialize payloadsData payloadsTypes [expectedLabel] partner argsName fooName
   
        let exprDes = 
            Expr.Sequential(<@@ printing "METHOD USED : Receive + Label = " label @@>,exprDes)
                                                            
        let cachingSupported = 
            payloadsTypes 
            |> List.filter (fun x -> x <> "System.Int32") 
            |> List.length 
            |> (fun x -> x=0) 

        if cachingSupported then         
            let addToCacheExpr = 
                <@@ let myValues:Buf<int> [] = (%%(Expr.NewArray(typeof<Buf<int>>, payloadsData)):Buf<int> []) 
                    Regarder.addVarsBufs "cache" payloadsTypes myValues @@>

            let exprDes = Expr.Sequential(exprDes, addToCacheExpr)                                                            
            let cachePrintExpr = <@@ Regarder.printCount "cache" @@>
        
            let exprDes = Expr.Sequential(exprDes, cachePrintExpr)
            Expr.Sequential(exprDes,stateInstanciation) 
        else 
            Expr.Sequential(exprDes,stateInstanciation) 


    // TODO : to implement
    let invokeCodeOnChoice (_:Delimiters) (Transitions _ ) (_:Expr list) = 

        let listPayload = [""]
        let listExpectedMessagesAndTypes = []
        let listExpectedMessages = listExpectedMessagesAndTypes |> List.map fst
        let listExpectedTypes = listExpectedMessagesAndTypes |> List.map snd |> List.map (fun _ -> [])
        let role = ""
    
        <@@ 
            printing "Before Branching : " (listExpectedMessages,listExpectedTypes,listPayload)
            let result = Regarder.receiveMessage "agent" listExpectedMessages role listExpectedTypes 
            let decode = new UTF8Encoding() 
            let labelRead = decode.GetString(result.[0]) 
            let assembly = System.Reflection.Assembly.GetExecutingAssembly() 
            let label = Regarder.getLabelType labelRead 
            let typing = assembly.GetType(label.FullName) 
            System.Activator.CreateInstance(typing,[||]) 
        @@>

    // TODO : to implement
    let invokeCodeOnBranchedLabel (transition:Transition) (stateInstanciation:Expr) (args:Expr list) =
        let payloadBuffers  = args.Tail.Tail
        let payloadsTypes   = 
            let (Payloads payloads ) = transition.Payloads
            [ for (Payload (_, ( PType pType))) in payloads -> pType ]
        let (Assertion assertion) = transition.Assertion

        let fooName,argsName = 
            if ((assertion <> "fun expression -> expression") && (assertion <> ""))  then
                let index = RefinementTypes.dictFunInfos.Count                                                            
                let assertion = RefinementTypes.createFnRule index assertion
                assertion |> fst |> RefinementTypes.addToDict
                snd assertion 
            else 
                "",[]

        let exprDes = deserializeChoice payloadBuffers payloadsTypes argsName fooName
        Expr.Sequential(exprDes,stateInstanciation)

