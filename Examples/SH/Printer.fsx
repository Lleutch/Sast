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
    Provided.TypeProviderFile<"../../../Examples/SH/ShFSM_P.txt" // Fully specified path to the scribble file
                               ,"SH" // name of the protocol
                               ,"P" // local role
                               ,"../../../Examples/SH/configP.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.File // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ,ExplicitConnection = true>
let numIter = 3
let P = SH.P.instance
let S = SH.S.instance
(*
let rec fibrec a b iter (c:Fib.State13) = 
            let res = new DomainModel.Buf<int>()
            printfn "number of iter: %d" (numIter - iter)
            //let c = c0.sendHELLO(S, a)
            match iter with
                |0 -> 
                    let c1 = c.sendBYE(S)
                    let c2 = c1.receiveBYE(S)
                    c2.finish()
                |n -> 
                    let c1 = c.sendADD(S, a)
                    let c2 = c1.receiveRES(S, res)

                    printfn "Fibo : %d" (res.getValue())
                    Async.RunSynchronously(Async.Sleep(1000))

                    fibrec b (res.getValue()) (n-1) c2
*)

let rec printPoints (c:SH.State30) =
    let res = new DomainModel.Buf<int>()    
    match c.branch() with 
    | :? SH.addP as point -> 
        let c1 = point.receive(S, res)
        printPoints c1  
    | :? SH.none as none->  
        printPoints none.receive(S)
    | :? SH.close as close ->  

let sh = new SH()
printPoints (sh.Start())




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


    
