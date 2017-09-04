#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "vertex", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "BothInOrOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "addP", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "none", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "plane", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Itersection", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

// C:/cygwin64/home/rhu/code/vs/scribble/github.com/rumineykova/Sast/Examples/Fibonacci/
type SH = 
    Provided.TypeProviderFile<"../../../Examples/SH/ShFSM_C.txt" // Fully specified path to the scribble file
                               ,"SH" // name of the protocol
                               ,"C" // local role
                               ,"../../../Examples/SH/configC.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.File // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ,ExplicitConnection = true>
let numIter = 3
let S = SH.S.instance

let ifInteresect x y = x + y
let getInteresectionP x y = x + y 

let rec calcClipPoints (vert: int list)  (c:SH.State25) =
    let res1 = new DomainModel.Buf<int>()    
    let res2 = new DomainModel.Buf<int>()
    match c.branch() with 
    | :? SH.close as close -> close.receive(S).finish()            
    | :? SH.vertex as vertex -> 
        let c1 = vertex.receive(S, res1).receivevertex(S, res2)
        let v = ifInteresect(res1.getValue(), res2.getValue())
        match v with 
        | 1 | 0  -> c1.sendBothInOrOut(S, v)
        | _ -> let p = getInteresectionP(res1.getValue(), res2.getValue())
               c1.sendItersection(p) 
    

let polygon = []
let res = new DomainModel.Buf<int>()    
let sh = new SH()
let c = sh.Start().receiveplane(S, res)




//first |> fibrec 1 1 numIter

(*let rec fibrec a b iter (c0:Fib.State7) =
    let res = new DomainModel.Buf<int>()
    printfn"number of iter: %d" (numIter - iter)
    let c = c0.sendHELLO(S, a)


    
    match iter with
        |0 -> c.sendBYE(S).receiveBYE(S).finish()
        |n -> let c1 = c.sendADD(S, a)
              let c2 = c1.receiveRES(S, res)
              printfn "Fibo : %d" (res.getValue())
              Async.RunSynchronously(Async.Sleep(1000))
              fibrec b (res.getValue()) (n-1) c2
*)


//let check func dict = 


    
