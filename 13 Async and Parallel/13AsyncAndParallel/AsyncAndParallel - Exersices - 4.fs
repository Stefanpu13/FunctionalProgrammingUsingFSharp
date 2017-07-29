namespace Exersices4
open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing
module E = 
    (* 3.
        Make a quiz program where a user should guess a number by asking the following questions:
        • Is the number < n?
        • Is the number = n?
        • Is the number > n?
        where n is a integer. The program can give the following answers:
        • Yes
        • No
        • You guessed it!
        The program must fix a random number between 0 and 59 to be guessed before starting the
        dialogue, and each run of the program should give a new number to be guessed.
    *)

    // An asynchronous event queue kindly provided by Don Syme
    type AsyncEventQueue<'T>() =
        let mutable cont = None
        let queue = System.Collections.Generic.Queue<'T>()
        let tryTrigger() =
            match queue.Count, cont with
            | _, None -> (); printfn "'tryTrigger': no cont"
            | 0, _ -> ();  printfn "'tryTrigger': queue empty"
            | _, Some d ->
                cont <- None
                d (queue.Dequeue())

        let tryListen(d) =
            printfn "trylisten"
            if cont.IsSome then invalidOp "multicast not allowed"
            cont <- Some d
            tryTrigger()
        member x.Post msg = 
            printfn "3. continuation is triggered from Post with msg: %A" msg        
            queue.Enqueue msg; tryTrigger()
        member x.Receive() =
            Async.FromContinuations (fun (cont,econt,ccont) ->
                printfn "1. continuation is added to queue with 'trylisten': %A" cont
                tryListen cont)

    // ----------------- Dialog program------------------

    // The window part
    let window =
        new Form(Text="Web Source Length", Size=Size(925,225))
    let myGuessBox =
        new TextBox(Location=Point(50,25),Size=Size(400,25))
    let ansBox = new TextBox(Location=Point(150,150),Size=Size(200,25))
    let isSmallerButton =
        new Button(Location=Point(50,65),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Is the number < ")
    let isEqualButton =
        new Button(Location=Point(250,65),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Is the number = ")
    let isLargerButton =
        new Button(Location=Point(450,65),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Is the number > ")
    let playAgainButton =
        new Button(Location=Point(700,65),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Play again?")                    

    let guessButtons = [isSmallerButton;isEqualButton;isLargerButton] 
    let disable bs = 
        for (b:Button) in bs do
            b.Enabled <- false        

    // The dialogue part from Table 13.7 belongs here
    type Message =  
        | Start of string | Clear | Cancel 
        | Web of string | Error | Cancelled

    let ev = AsyncEventQueue()
    let rand = Random()
    

    let rec init ()=
        let numToGuess = rand.Next(60) 
        printfn "Number to guess is: %i" numToGuess
        playAgainButton.Enabled <- false
        for b in guessButtons do b.Enabled <- true
        myGuessBox.Text <- "30"
        ansBox.Text <- ""        

        async {
            return! waitForGuess numToGuess
        }
    and smallerOutcome guessedNum numToGuess = 
        async{
            let answer = if guessedNum > numToGuess then "yes" else "no"
            ansBox.Text <- answer            

            return! waitForGuess numToGuess
        }     
    and equalOutcome guessedNum numToGuess = 
        async{
            if guessedNum = numToGuess 
            then 
                ansBox.Text <- " You guessed it!"
                return! endGame () 
            else 
                ansBox.Text <-"no"
                return! waitForGuess numToGuess            
        }     
    and largerOutcome guessedNum numToGuess = 
        async{
            let answer = if guessedNum < numToGuess then " Yes" else "no"
            ansBox.Text <- answer            

            return! waitForGuess numToGuess
        }   
    and showOutcome guess numToGuess =   
        async {
            match guess with 
            | Smaller n -> return! smallerOutcome n numToGuess
            | Equal n -> return! equalOutcome n numToGuess
            | Larger n -> return! largerOutcome n numToGuess
        }
    and waitForGuess numToGuess = 
        async {
            let! guess = ev.Receive() 
            return! showOutcome guess numToGuess
        }     
    and endGame () = 
        async{
            disable [isSmallerButton;isEqualButton;isLargerButton]
            playAgainButton.Enabled <-true
        }     

    // Initialization
    window.Controls.Add myGuessBox
    window.Controls.Add ansBox
    window.Controls.Add isSmallerButton
    window.Controls.Add isEqualButton
    window.Controls.Add isLargerButton
    window.Controls.Add playAgainButton
    isSmallerButton.Click.Add (fun _ -> ev.Post (Smaller (int myGuessBox.Text)))
    isEqualButton.Click.Add (fun _ -> ev.Post (Equal (int myGuessBox.Text)))
    isLargerButton.Click.Add (fun _ -> ev.Post (Larger (int myGuessBox.Text)))
    playAgainButton.Click.Add(fun _ -> Async.StartImmediate (init()))
    // Start
    Async.StartImmediate (init())
    window.Show()
