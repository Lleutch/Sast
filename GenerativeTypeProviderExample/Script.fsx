#r "./bin/Debug/GenerativeTypeProviderExample.dll"

open GenerativeTypeProviderExample.Provided
open GenerativeTypeProviderExample

type Test = Provided.TypeProvider<"Testons">
let test = Test.MyProperty
let autre = new Test()
Test.
let c = autre.MyMethod()