module ExercisesTests
open NUnit.Framework
open FsUnitTyped
open Values.E


[<TestFixture>]
type ``f tests``() = 
    [<Test>]
    member test.``When input are 24, 27, 29, 30, outputs should be "true, true, false, false", respecctively ``() = 
      f(24) |> shouldEqual true
      f(27) |> shouldEqual true
      f(29) |> shouldEqual false
      f(30) |> shouldEqual false

// pow tests
[<TestFixture>]
type ``pow tests``() = 
    [<Test>]
    member test.``When input is 0 or negative, outputs should be empty string``() = 
      pow ( "a", -4) |> shouldEqual ""
      pow ( "a", 0) |> shouldEqual ""

    [<Test>]
    member test.``When input is ("a", 3), outputs should be "aaa"``() = 
      pow ( "a", 3) |> shouldEqual "aaa"

// isIthChar tests
[<TestFixture>]
type ``isIthChar tests``() = 
    [<Test>]
    member test.``When input is 0 or negative, outputs should be empty string``() = 
      pow ( "a", -4) |> shouldEqual ""
      pow ( "a", 0) |> shouldEqual ""

    [<Test>]
    member test.``When input is ("a", 3), outputs should be "aaa"``() = 
      pow ( "a", 3) |> shouldEqual "aaa"
      