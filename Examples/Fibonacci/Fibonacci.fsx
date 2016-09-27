#r "../../GenerativeTypeProviderExample/bin/Debug/GenerativeTypeProviderExample.dll"

open GenerativeTypeProviderExample
                        
[<Literal>]
let delims = """ [ {"label" : "fib", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "bye", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } } ] """


(*type Fib = Provided.TypeProviderFile<"Examples/Fibonacci/Fibonacci.scr","demo.Fibonacci","A","Examples/Fibonacci/config.yaml",Delimiter=delims>


let b = Fib.B.instance
let buf = new DomainModel.Buf<int>()

let rec fibonacci numIter v1 v2 (state1:Fib.State1) =
    match numIter with 
        |0 -> state1.sendbye(b).finish()
        |n -> let nextIter = state1.sendfib(b,v1,v2).receivefib(b,buf)
              printfn "Fibo : %d" (buf.getValue())
              Async.RunSynchronously(Async.Sleep(1000))
              fibonacci (numIter-1) (v2) (buf.getValue()) nextIter  

let instance = new Fib()
instance.Start() |> fibonacci (10-2) 1 1 *)
 




type Fib = Provided.TypeProviderFile<"Examples/Fibonacci/Fibonacci.scr","demo.Fibonacci","A","Examples/Fibonacci/config.yaml",Delimiter=delims>

let numIter = 10-2
let B = Fib.B.instance

let rec fibrec a b iter (c:Fib.State1) =
    let res = new DomainModel.Buf<int>()
    printfn"number of iter: %d" (numIter - iter)
    match iter with
        |0 -> c.sendbye(B).finish()
        |n -> let c1 = c.sendfib(B,a,b)
              let c2 = c1.receivefib(B,res)
              printfn "Fibo : %d" (res.getValue())
              Async.RunSynchronously(Async.Sleep(1000))
              fibrec b (res.getValue()) (n-1) c2


let fibo = new Fib()
let first = fibo.Start()
first |> fibrec 1 1 numIter