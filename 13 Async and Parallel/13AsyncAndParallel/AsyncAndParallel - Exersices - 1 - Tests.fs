module Exercises1Tests
#if INTERACTIVE
#I @"bin\Debug"
#r "nunit.framework.dll"
#r  "FsUnit.NUnit.dll"
#endif

open NUnit.Framework
open FsUnitTyped
open Exersices1.E

// Test odd numbers
[<TestFixture>]
type ``Test odd numbers``() =       

    [<Test>]
    member t.``Random sequence element should be odd`` () = 
        3 |> shouldEqual <| 3