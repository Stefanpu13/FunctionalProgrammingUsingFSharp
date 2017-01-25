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
    member test.``When input is ("a", 3), output should be "aaa"``() = 
      pow ( "a", 3) |> shouldEqual "aaa"

// isIthChar tests
[<TestFixture>]
type ``isIthChar tests``() = 
    [<Test>]
    member test.``When input is negative or bigger than string length, output should be false``() = 
      isIthChar ( "abc", -1, 'a') |> shouldEqual false
      isIthChar ( "abc", 4, 'a') |> shouldEqual false

    [<Test>]
    member test.``When input is ("abc", 3, 'a'), outputs should be false``() = 
      isIthChar ("abc", 3, 'a')  |> shouldEqual false

    
    [<Test>]
    member test.``When input is ("abc", 0, 'a'), outputs should be true``() = 
      isIthChar ("abc", 0, 'a')  |> shouldEqual true

// occFromIth tests
[<TestFixture>]
type ``occFromIth tests``() = 
    [<Test>]
    member test.``When input is negative or bigger than string length, output should be 0``() = 
      occFromIth ( "abc", -1, 'a') |> shouldEqual 0
      occFromIth ( "abc", 4, 'a') |> shouldEqual 0
      occFromIth2 ( "abc", -1, 'a') |> shouldEqual 0
      occFromIth2 ( "abc", 4, 'a') |> shouldEqual 0

    [<Test>]
    member test.``When input is ("abc", 2, 'a'), outputs should be 0``() = 
      occFromIth ("abc", 2, 'a')  |> shouldEqual 0
      occFromIth2 ("abc", 2, 'a')  |> shouldEqual 0

    [<Test>]
    member test.``When input is ("abc", 2, 'c'), outputs should be 1``() = 
      occFromIth ("abc", 2, 'c')  |> shouldEqual 1
      occFromIth2 ("abc", 2, 'c')  |> shouldEqual 1
    
    [<Test>]
    member test.``When input is ("abcab", 0, 'a'), outputs should be 2``() = 
      occFromIth ("abcab", 0, 'a')  |> shouldEqual 2
      occFromIth2 ("abcab", 0, 'a')  |> shouldEqual 2

// notDivisible tests
[<TestFixture>]
type ``notDivisible tests``() = 
    [<Test>]
    member test.``When divisor is 0, output should be true``() = 
      notDivisible(0, 9) |> shouldEqual true

    [<Test>]
    member test.``When input is (2, 5), output should be true``() = 
      notDivisible(2, 5) |> shouldEqual true

    [<Test>]
    member test.``When input is (3, 9), output should be false``() = 
      notDivisible(3, 9) |> shouldEqual false
      