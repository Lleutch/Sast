#r "./bin/Debug/GenerativeTypeProviderExample.dll"

open GenerativeTypeProviderExample.Provided
open GenerativeTypeProviderExample

type Test = Provided.TypeProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"Hello()" , "type":"send" , "nextState": 4},
    { "currentState":4 , "localRole":"Me", "partner":"You" , "label":"GoodMorning()", "type":"receive" , "nextState":2  },
    { "currentState":2 , "localRole":"Me", "partner":"You" , "label":"Bye()", "type":"send" , "nextState":3  }  ] """>

let test = new Test()
let truc = test.Start()
