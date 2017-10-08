module ScribbleGenerativeTypeProvider.ScribbleParser

//#r "./packages/FSharp.Data/lib/net40/FSharp.Data.dll"
//#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
//#r "./packages/FParsec/lib/net40-client/FParsec.dll"
open FSharp.Data
open System
open System.Text.RegularExpressions
open FParsec
open Common.BasicFSM
open Common.CommonFSM
open Common.ConversionFSM


/// TODO : Add Error Handling there
/// This parser is not working. We need to remove it for the future.
/// Kept for compilation purpose for the moment. 
module parserHelper =
    let brackets = ('{','}')
    let squareBrackets = ('[',']')
    let quotes = ('\"','\"')
    let str_ws s = spaces >>. pstring s .>> spaces
    let char_ws c = pchar c .>> spaces
    let anyCharsTill pEnd = manyCharsTill anyChar pEnd
    let anyCharsTillApply pEnd f = manyCharsTillApply anyChar pEnd f
    let quoted quote parser = 
        pchar (quote |> fst) 
        .>> spaces >>. parser .>> spaces 
        .>> pchar (quote |> snd) 
    let line:Parser<string,unit> = anyCharsTill newline
    let restOfLineAfter str = str_ws str >>. line
    let startUpUseless:Parser<_,unit> = 
        pstring "compound = true;" 
        |> anyCharsTill
        // >>. skipNewline 
         
    let currentStateId :Parser<_,unit> = 
        spaces 
        >>. quoted quotes pint32 .>> spaces 
        |>> StateId
    let nextStateId:Parser<_,unit> = 
        spaces 
        >>. quoted quotes pint32 .>> spaces 
        |>> StateId
    
    let partnerEvent:Parser<_,unit> =
        str_ws "label"
        >>. pstring "=" >>. spaces
        >>. pchar '\"'
        >>. (anyCharsTillApply (pchar '!' <|> pchar '?') (fun str event -> (str,event)))
        |>> fun (str,event) -> 
                match event with
                | '!' -> 
                    Partner(str),EventType.Send
                | '?' ->
                    Partner(str),EventType.Receive
                | _ ->
                    failwith "This case can never happen, if these two weren't here the flow would
                    have been broken earlier!!"

    let label:Parser<_,unit> = 
        spaces
        >>. (anyCharsTill (pchar '('))
        |>> Label

    // TODO: Either re-implement or remove the whole file.
    let payloads:Parser<_,unit> =
//        let singlePayload =
//            spaces
//            >>. manyChars (noneOf [',';')'])
//        spaces
//        >>. between 
//                spaces 
//                (pstring ")\"" >>. spaces >>. pstring "];" >>. spaces) 
//                (sepBy singlePayload (pstring ",")) 
//        |>> Payloads        
        spaces
        |>> (fun () -> Payloads [])


    let event role currentState =
        parse{
            let! _ = pstring "->"
            let! nextState = nextStateId
            let! _ = pstring "["
            let! partner,eventType = partnerEvent
            let! label = label
            let! payloads = payloads
            return 
                {   Assertion   = Assertion ""
                    Current     = currentState
                    LocalRole   = LocalRole role
                    Partner     = partner
                    Label       = label
                    Payloads    = payloads     
                    EventType   = eventType
                    Next        = nextState
                } |> Some
        }

    let skipLabelInfoLine:Parser<Event option,unit> =
         parse{
            let! _ = pstring "[" .>> spaces
            let! _ = manyCharsTill anyChar (pstring "];")
            let! _ = spaces
            return None
        }

    let transitionOrSkipping role =
        parse{
            let! _ = spaces
            let! currentState = currentStateId .>> spaces
            return! event role currentState <|> skipLabelInfoLine
        }

    let events role = 
        parse{
            let! _ = startUpUseless
            do! spaces
            let! list = (many (transitionOrSkipping role)) 
            return 
                list 
                |> List.filter Option.isSome 
                |> List.map Option.get
                |> Events
        }
module Parsing = 
    open parserHelper
    type ScribbleAPI = FSharp.Data.JsonProvider<""" { "code":"Code", "proto":"global protocol", "role":"local role" } """>
    type DiGraph = FSharp.Data.JsonProvider<""" {"result":"value"} """>
    type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":0 , "localRole":"StringLocalRole" , "partner":"StringPartner" , "label":"StringLabel" , "payload":["StringTypes"] , "type":"EventType" , "nextState":0  } ] """>
                        

    let isCurrentChoice (fsm:ScribbleProtocole.Root []) (index:int) =
        let current = fsm.[index].CurrentState
        let mutable size = 0 
        for elem in fsm do
            if elem.CurrentState = current then
                size <- size + 1
        (size>1)

    let modifyAllChoice (fsm:ScribbleProtocole.Root []) =
        let mutable newArray = [||] 
        for i in 0..(fsm.Length-1) do
            let elem = fsm.[i]
            if elem.Type = "receive" && (isCurrentChoice fsm i) then
                let newElem = ScribbleProtocole.Root(elem.CurrentState,elem.LocalRole,elem.Partner,elem.Label,elem.Payload,"choice",elem.NextState)
                newArray <- Array.append newArray [|newElem|]            
            else
            newArray <- Array.append newArray [|elem|]
        newArray


    let getArrayJson (response:string) json =
        //let s = DiGraph.Parse(response)
        //let s0 = s.Result
        let s0 = response
        match Regex.IsMatch(s0,"java\\.lang\\.NullPointerException") with
        |true ->  
            // TODO : Add correct Error
            failwith "getArrayJson"
        |false ->
            let role = ScribbleAPI.Parse(json)
            let PFsm = run (events (role.Role) ) s0
            match PFsm with
            | Failure (error,_,_) -> failwith error
            | Success (fsm,_,_) -> traverseStateMachine fsm


    let getFSMJson (json:string) = getArrayJson json

