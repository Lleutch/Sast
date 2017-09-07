namespace AssertionParsing

open AssertionParsing.AssertionParser
open AssertionParsing.Visitors

module FuncGenerator =
    let printVarSeq (setV: Set<string>) = setV |> Set.toList  |> String.concat " " 

    let genLambdaFromExpr expr = 
        let args = getVars expr
        let body = getStringRepr expr
        if (body <> "") then 
            sprintf "fun %s -> %s" (printVarSeq args) body
        else body 
    
    let genLambdaFromStr expr =
       match (parse expr) with 
        | Some res -> genLambdaFromExpr res 
        | None -> ""
    