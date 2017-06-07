module Exercises4Tests
#if INTERACTIVE
#I @"bin\Debug"
// #r "nunit.framework.dll"
#r  "FsUnit.NUnit.dll"
#endif

open NUnit.Framework
open FsUnitTyped
open System.IO
open Exercises4.E

// Test getPosition
[<TestFixture>]
type ``Test getPosition``() =

    [<Test>]
    member t.``If valid position input is entered, output should be successfully parsed`` () = 
        let input =  """14*27'35.03" N 55*1'47" E"""
        getPosition input |> shouldEqual <| 
            ValidCoords (14.0*60.0*60.0 + 27.0*60.0 + 35.03, 55.0*3600.0 + 1.0*60.0+47.0)

    [<Test>]
    member t.``If valid input is entered and lat is S and lgt is W, output should parse them as negative numbers`` () = 
        let input =  """14*59'59.03" S 55*1'47" W"""
        getPosition input |> shouldEqual <| 
            ValidCoords (-1.0 * (14.0*60.0*60.0 + 59.0*60.0 + 59.03), -1.0 * (55.0*3600.0 + 1.0*60.0+47.0))

    [<Test>]
    member t.``If lat and lgt are swapped, result should be invalid`` () = 
        let validInput =  """14*59'59.03" S 55*1'47" W"""
        getPosition validInput |> shouldEqual <| 
            ValidCoords (-1.0 * (14.0*60.0*60.0 + 59.0*60.0 + 59.03), -1.0 * (55.0*3600.0 + 1.0*60.0+47.0))
        let invalidInput = """14*59'59.03" W 55*1'47" S"""
        getPosition invalidInput |> shouldEqual <| InvalidCoords

    [<Test>]
    member t.``If degrees are outside range, result should be invalid`` () = 
        let validInput =  """14*59'59.03" S 55*1'47" W"""
        getPosition validInput |> shouldEqual <| 
            ValidCoords (-1.0 * (14.0*60.0*60.0 + 59.0*60.0 + 59.03), -1.0 * (55.0*3600.0 + 1.0*60.0+47.0))
        let invalidInputLatSmaller = """-14*59'59.03" S 55*1'47" W"""
        getPosition invalidInputLatSmaller |> shouldEqual <| InvalidCoords

        let invalidInputLatBigger = """184*59'59.03" N 55*1'47" W"""
        getPosition invalidInputLatBigger |> shouldEqual <| InvalidCoords

        let invalidInputLgtSmaller = """14*59'59.03" N -55*1'47" E"""
        getPosition invalidInputLgtSmaller |> shouldEqual <| InvalidCoords

        let invalidInputLgtBigger = """14*59'59.03" S 192*1'47" W"""
        getPosition invalidInputLgtBigger |> shouldEqual <| InvalidCoords

    [<Test>]
    member t.``If degrees are not integers, result should be invalid`` () = 
        let validInput =  """14*59'59.03" S 55*1'47" W"""
        getPosition validInput |> shouldEqual <| 
            ValidCoords (-1.0 * (14.0*60.0*60.0 + 59.0*60.0 + 59.03), -1.0 * (55.0*3600.0 + 1.0*60.0+47.0))
        let invalidInputLatDouble = """1.04*59'59.03" S 55*1'47" W"""
        getPosition invalidInputLatDouble |> shouldEqual <| InvalidCoords

        let invalidInputLgtDouble = """14*59'59.03" S 12.6*1'47" W"""
        getPosition invalidInputLgtDouble |> shouldEqual <| InvalidCoords

    [<Test>]
    member t.``If minutes are outside range, result should be invalid`` () = 
        let validInput =  """14*59'59.03" S 55*1'47" W"""
        getPosition validInput |> shouldEqual <| 
            ValidCoords (-1.0 * (14.0*60.0*60.0 + 59.0*60.0 + 59.03), -1.0 * (55.0*3600.0 + 1.0*60.0+47.0))
        let invalidInputLatSmaller = """14*-59'59.03" S 55*1'47" W"""
        getPosition invalidInputLatSmaller |> shouldEqual <| InvalidCoords

        let invalidInputLatBigger = """14*69'59.03" N 55*1'47" W"""
        getPosition invalidInputLatBigger |> shouldEqual <| InvalidCoords

        let invalidInputLgtSmaller = """14*59'59.03" N 55*-1'47" E"""
        getPosition invalidInputLgtSmaller |> shouldEqual <| InvalidCoords

        let invalidInputLgtBigger = """14*59'59.03" S 55*61'47" W"""
        getPosition invalidInputLgtBigger |> shouldEqual <| InvalidCoords

    [<Test>]
    member t.``If minutes are not integers, result should be invalid`` () = 
        let validInput =  """14*59'59.03" S 55*1'47" W"""
        getPosition validInput |> shouldEqual <| 
            ValidCoords (-1.0 * (14.0*60.0*60.0 + 59.0*60.0 + 59.03), -1.0 * (55.0*3600.0 + 1.0*60.0+47.0))
        let invalidInputLatDouble = """1.*59.01'59.03" S 55*1'47" W"""
        getPosition invalidInputLatDouble |> shouldEqual <| InvalidCoords

        let invalidInputLgtDouble = """14*59'59.03" S 12*1.55'47" W"""
        getPosition invalidInputLgtDouble |> shouldEqual <| InvalidCoords


    [<Test>]
    member t.``If seconds are outside range, result should be invalid`` () = 
        let validInput =  """14*59'59.03" S 55*1'47" W"""
        getPosition validInput |> shouldEqual <| 
            ValidCoords (-1.0 * (14.0*60.0*60.0 + 59.0*60.0 + 59.03), -1.0 * (55.0*3600.0 + 1.0*60.0+47.0))
        let invalidInputLatSmaller = """14*59'-59.03" S 55*1'47" W"""
        getPosition invalidInputLatSmaller |> shouldEqual <| InvalidCoords

        let invalidInputLatBigger = """14*59'69.03" N 55*1'47" W"""
        getPosition invalidInputLatBigger |> shouldEqual <| InvalidCoords

        let invalidInputLgtSmaller = """14*59'59.03" N 55*1'-47" E"""
        getPosition invalidInputLgtSmaller |> shouldEqual <| InvalidCoords

        let invalidInputLgtBigger = """14*59'59.03" S 55*1'147" W"""
        getPosition invalidInputLgtBigger |> shouldEqual <| InvalidCoords

    [<Test>]
    member t.``If letter other than N|S|W|E is used, result should be invalid`` () = 
        let validInput =  """14*59'59.03" S 55*1'47" W"""
        getPosition validInput |> shouldEqual <| 
            ValidCoords (-1.0 * (14.0*60.0*60.0 + 59.0*60.0 + 59.03), -1.0 * (55.0*3600.0 + 1.0*60.0+47.0))
        let invalidInputLatSmallLetter = """14*59'59.03" s 55*1'47" W"""
        getPosition invalidInputLatSmallLetter |> shouldEqual <| InvalidCoords

        let invalidInputLgtSmallLetter = """14*59'69.03" N 55*1'47" w"""
        getPosition invalidInputLgtSmallLetter |> shouldEqual <| InvalidCoords

        let invalidInputLatLetter = """14*59'59.03" a 55*1'-47" E"""
        getPosition invalidInputLatLetter |> shouldEqual <| InvalidCoords

        let invalidInputLgtBigger = """14*59'59.03" S 55*1'147" x"""
        getPosition invalidInputLgtBigger |> shouldEqual <| InvalidCoords