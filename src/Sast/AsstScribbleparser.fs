module ScribbleGenerativeTypeProvider.AsstScribbleParser

open System.Text.RegularExpressions
open FParsec
open Common
open Common.CommonFSM
open Common.BasicFSM
open Common.ConversionFSM


let nextNumber = 
    let counter = ref 0
    fun () -> 
        counter.Value <- !counter + 1
        !counter



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
        >>. skipNewline 
    let isUnderscore c = c='_'                                          
    let isVar c = isLetter c || isDigit c || c='_'
    //let dummy = many1Satisfy2L isUnderscore isVar "identifier" .>> spaces          
    let varParser:Parser<_, unit> = 
        many1Satisfy isVar .>> spaces
    let expr:Parser<_, unit> = 
        let normalChar 
            = satisfy (fun c -> c <> ';' && c<>'\\' && c<>'\"')
        (manyChars normalChar) 
    // pstring "\"" .>> spaces .>> pstring "];" >>. spaces

    let currentStateId:Parser<_,unit> = 
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
        >>. (anyCharsTillApply (attempt (pstring "!!" <|> pstring "!" <|> pstring "??" <|> pstring "?")) (fun partner event -> (partner,event)))
        |>> fun (partner,event) -> 
                match event with
                | "!!" -> 
                    Partner(partner),EventType.Request
                | "??" -> 
                    Partner(partner),EventType.Accept
                | "!" -> 
                    Partner(partner),EventType.Send
                | "?" ->
                    Partner(partner),EventType.Receive                
                | _ ->
                    failwith "This case can never happen, if these two weren't here the flow would
                    have been broken earlier!!"

    let label:Parser<_,unit> = 
        spaces
        >>. (anyCharsTill (pchar '('))
        |>> Label
    
    let payloads:Parser<_,unit> =
        //let varPayload = spaces >>. manyChars (noneOf [',';')';':'])
        //let typePayload = spaces >>. manyChars (noneOf [',';')'])
        //let singlePayload = pipe3 varPayload (pstring ":") (spaces >>. varPayload) (fun id _ str ->  (id, str))
        let varName = manyChars (noneOf [',';')'; ':']) 
        let unitType = ((pstring "_" >>. varName ) <|> (pstring "Unit")) |>> (fun _ -> "")
        let _ = (pstring "_" >>. varName ) //|>> (fun x -> "")
        let singlePayload = 
             attempt
                (pipe4 (spaces >>.  varName) (pstring ":") spaces (unitType <|> varName)
                        (fun name _ _ varType -> 
                                if (varType<>"") 
                                        then  (sprintf "%s" name, sprintf "%s" varType) 
                                else sprintf "", ""))
                <|> ((spaces >>. (unitType <|> varName)) |>> 
                     (fun varType -> 
                        if (varType<>"") then  (sprintf "_dummy%i" (nextNumber()), sprintf "%s" varType) 
                        else sprintf "", ""))

        spaces  >>. (sepBy singlePayload (pstring ",")) .>> (pstring ")")
        |>> (fun payloadList -> 
                payloadList
                |> List.map(fun (varName,varType) ->
                    let varName = PName varName
                    let varType = PType varType
                    Payload (varName,varType)
                   )
                |> Payloads
            )
                   
    //(pstring ")\"" >>. spaces >>. pstring "];" >>. spaces)    
    let assertion:Parser<_,unit> =
        let endOfPayload = pstring "\"" >>. spaces >>. pstring "];" >>. spaces
        ((endOfPayload |>> fun _ -> "") <|> ((pstring "@" >>. (between (pstring "\\\"")  (pstring "\\\"") expr))  .>> endOfPayload)) 
        |>> Assertion

    let event role currentState =
        parse{
            let! _ = pstring "->"
            let! nextState = nextStateId
            let! _ = pstring "["
            let! partner,eventType = partnerEvent
            let! label = label
            let! payloads = payloads
            let! assertion = assertion
            return 
                {
                    Current     = currentState
                    LocalRole   = LocalRole role
                    Partner     = partner
                    Label       = label
                    Payloads    = payloads
                    Assertion   = assertion      
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
    let eventOrSkipping role =
        parse{
            let! _ = spaces
            let! currentState = currentStateId .>> spaces
            return! event role currentState <|> skipLabelInfoLine
        }
    let events role = 
        parse{
            let! _ = startUpUseless
            do! spaces
            let! list = (many (eventOrSkipping role)) 
            printfn "%A" list
            return 
                list 
                |> List.filter Option.isSome 
                |> List.map Option.get
                |> Events
        }
module Parsing = 
    open parserHelper

    // TODO : Find a way to get rid of this TP (via response/json combo in getArrayJson function)
    type ScribbleAPI = FSharp.Data.JsonProvider<""" { "code":"Code", "proto":"global protocol", "role":"local role" } """>


    // TODO : Update with correct Error Handling
    let getArrayJson (response:string) json =
        let s0 = response
        match Regex.IsMatch(s0,"java\\.lang\\.NullPointerException") with
        |true -> failwith "The file given does not contain a valid fsm"
        |false ->
            let role = ScribbleAPI.Parse(json)
            let Pfsm = run (events (role.Role) ) s0
            match Pfsm with
            | Failure (_,_,_) -> failwith "The file given does not contain a valid fsm"
            | Success (fsm,_,_) -> traverseStateMachine fsm
                
    let getFSMJson (json:string) = getArrayJson json