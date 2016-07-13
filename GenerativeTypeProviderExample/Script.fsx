#r "./bin/Debug/GenerativeTypeProviderExample.dll"

open GenerativeTypeProviderExample.Provided
open GenerativeTypeProviderExample

type Test = Provided.TypeProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"World" , "label":"b()" , "type":"send" , "nextState": 5} ,
    { "currentState":5 , "localRole":"Me", "partner":"World" , "label":"a()" , "type":"receive" , "nextState": 3 } ,
    { "currentState":3 , "localRole":"Me", "partner":"Her" , "label":"e()", "type":"receive" , "nextState":4  },
    { "currentState":4 , "localRole":"Her", "partner":"You" , "label":"f()", "type":"choice" , "nextState":2  }, 
    { "currentState":4 , "localRole":"Her", "partner":"You" , "label":"g()", "type":"choice" , "nextState":2  }  ] """>
let test = Test.
let autre = new Test()
let c = autre.instanciate()