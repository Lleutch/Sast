#r "./bin/Debug/GenerativeTypeProviderExample.dll"

open System
open GenerativeTypeProviderExample
open GenerativeTypeProviderExample.Provided
open Microsoft.FSharp.Quotations

(*type Test = Provided.TypeProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"Hello()" , "type":"send" , "nextState": 4},
    { "currentState":4 , "localRole":"Me", "partner":"You" , "label":"GoodMorning()" , "payload": ["System.Int32"] , "type":"receive" , "nextState":2  },
    { "currentState":2 , "localRole":"Me", "partner":"You" , "label":"Bye()", "payload": ["System.Int32"] , "type":"send" , "nextState":3  }  ] """>

let test = new Test()
type Test = Provided.TypeProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"Hello()" , "type":"send" , "nextState": 5},
    { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"Naaa()" , "type":"send" , "nextState": 5},
    { "currentState":5 , "localRole":"Me", "partner":"You" , "label":"GoodMorning()" , "type":"choice" , "nextState":4  },
    { "currentState":5 , "localRole":"Me", "partner":"You" , "label":"Bye()" , "type":"choice" , "nextState":3  },
    { "currentState":4 , "localRole":"Me", "partner":"You" , "label":"ThankYou()", "type":"send" , "nextState":2 },
    { "currentState":3 , "localRole":"Me", "partner":"You" , "label":"YouToo()" , "type":"send" , "nextState":2  },
    { "currentState":2 , "localRole":"Me", "partner":"You" , "label":"Stop()" , "type":"send" , "nextState":6  } ] """>

let buf = new TypeGeneration.Buf<int>()
let c = test.Start()
//let d = c.send(new Test.Hello(),Test.You.instance).branch() .receive(new Test.GoodMorning(),Test.You.instance,buf).send(new Test.Bye(),Test.You.instance,buf.  //.branch() //.receive(new Test.GoodMorning(),Test.You.instance).send(new Test.Bye(), Test.You.instance)
*)

type Test = Provided.TypeProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"Hello()" , "payload" : ["System.Int32","System.String"] , "type":"send" , "nextState": 5},
    { "currentState":5 , "localRole":"Me", "partner":"You" , "label":"GoodMornin()" , "payload" : ["System.Int32"] , "type":"choice" , "nextState": 4},
    { "currentState":5 , "localRole":"Me", "partner":"You" , "label":"GoodNight()" , "payload" : ["System.Int32"] , "type":"choice" , "nextState": 4},
    { "currentState":2 , "localRole":"Me", "partner":"You" , "label":"Bye()", "payload" : ["System.Int32"] , "type":"send" , "nextState":3  }  ] """>

let test = new Test()
let c = test.Start()

let a = new DomainModel.Buf<int>()
let b = new DomainModel.Buf<float>()
let d = c.send(new Test.Hello(),Test.You.instance,23,"hey you").branch() //.receive(new Test.GoodMornin(),Test.You.instance,a).send(new Test.Bye(),Test.You.instance,a.getValue())

a.getValue()

match d with 
    | :? Test.Bye  as e -> e.send(new Test.Thankyou())
    | :? Test.Bye as e -> e.send(new Test.youToo())