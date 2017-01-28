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
      
// test tests
[<TestFixture>]
type ``test tests``() = 
    [<Test>]
    member test.``When a > b, output should be false``() = 
        noDivisorsInInterval (3, 0, 9) |> shouldEqual false

    [<Test>]
    member test.``When a = 3 b = 5, c = 17, output should be true``() = 
        noDivisorsInInterval (3, 5, 17) |> shouldEqual true

    [<Test>]
    member test.``When a = 3 b = 5, c = 18, output should be false``() = 
        noDivisorsInInterval (3, 5, 18) |> shouldEqual false
      
    [<Test>]
    member test.``When a = 3 b = 5, c = 10, output should be false``() = 
        noDivisorsInInterval (3, 5, 10) |> shouldEqual false

    [<Test>]
    member test.``When a = 3 b = 5, c = 16, output should be false``() = 
        noDivisorsInInterval (3, 5, 16) |> shouldEqual false

// prime tests
[<TestFixture>]
type ``prime tests``() = 
    [<Test>]
    member test.``When input is < 2, output should be false``() = 
        prime -1 |> shouldEqual false

    [<Test>]
    member test.``When input is 23, output should be true``() = 
        prime 23 |> shouldEqual true

// nextPrime tests
[<TestFixture>]
type ``nextPrime tests``() = 
    [<Test>]
    member test.``When input is < 2, output should be 2``() = 
        nextPrime -1 |> shouldEqual 2

    [<Test>]
    member test.``When input is 23, output should be 29``() = 
        nextPrime 23 |> shouldEqual 29
    
// bin tests
[<TestFixture>]
type ``bin tests``() = 
    [<Test>]
    member test.``When n < k, output should be 0``() =         
        bin (0, 4) |> shouldEqual 0
        bin (1, 4) |> shouldEqual 0
        bin (2, 4) |> shouldEqual 0
        bin (3, 4) |> shouldEqual 0

    [<Test>]
    member test.``When input is (4, 0), output should be 1``() = 
        bin (4, 0) |> shouldEqual 1

    [<Test>]
    member test.``When input is (4, 4), output should be 1``() = 
        bin (4, 4) |> shouldEqual 1

    [<Test>]
    member test.``When input is (4, 6), output should be 0``() = 
        bin (4, 6) |> shouldEqual 0

    [<Test>]
    member test.``When input is (6, 4), output should be 0``() = 
        bin (6, 4) |> shouldEqual 15

// VAT tests
[<TestFixture>]
type ``VAT tests``() = 
    [<Test>]
    member test.``When input is 50 12.0, output should be 18.0``() = 
         VAT 50 12.0|> shouldEqual 18.0

    [<Test>]
    member test.``When input is 40 12.0, output should be 16.8``() = 
         VAT 40 12.0|> shouldEqual 16.8

    [<Test>]
    member test.``When input is -50 12.0, output should be 6.0``() = 
         VAT -50 12.0|> shouldEqual 6.0

    [<Test>]
    member test.``When input is 0 12.0, output should be 12.0``() = 
         VAT 0 12.0|> shouldEqual 12.0


// unVAT tests
[<TestFixture>]
type ``unVAT tests``() =      
    [<Test>]
    member test.``When input is 50 12.0, output should be 12.0``() = 
         unVAT 50 (VAT 50 12.0) |> shouldEqual 12.0

    [<Test>]
    member test.``When input is 40 12.0, output should be 12.0``() = 
        (* Convert to int to avoid rounding errors in floats. 
           See http://stackoverflow.com/questions/3205453/fsunit-and-checking-equality-of-floating-point-numbers
           for more info. 
        *)
        int (unVAT 40 (VAT 40 12.0)) |> shouldEqual 12

    [<Test>]
    member test.``When input is -50 12.0, output should be 12.0``() = 
        unVAT -50 (VAT -50 12.0) |> shouldEqual 12.0

    [<Test>]
    member test.``When input is 0 12.0, output should be 12.0``() = 
        unVAT 0 (VAT 0 12.0) |> shouldEqual 12.0

// min tests
[<TestFixture>]
type ``min tests``() = 
    [<Test>]
    member test.``When input is 50 12.0, output should be 18.0``() = 
         VAT 50 12.0|> shouldEqual 18.0

    [<Test>]
    member test.``When input is 40 12.0, output should be 16.8``() = 
         VAT 40 12.0|> shouldEqual 16.8

    [<Test>]
    member test.``When input is -50 12.0, output should be 6.0``() = 
         VAT -50 12.0|> shouldEqual 6.0

    [<Test>]
    member test.``When input is 0 12.0, output should be 12.0``() = 
         VAT 0 12.0|> shouldEqual 12.0