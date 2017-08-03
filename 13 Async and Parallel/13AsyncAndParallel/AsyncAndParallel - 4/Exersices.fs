namespace Exersices4
open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing

#if INTERACTIVE
#load "FormComponents.fs"
#load "../Async.fs"

// Open namespace, so other files/modules can be opened
open Exersices4
#endif
open FormComponents
open Async.Async

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


    // ----------------- Dialog program------------------
    let disable bs = 
        for (b:Button) in bs do
            b.Enabled <- false        
    
    let rand = Random()        
    
    // The dialogue part from Table 13.7 belongs here
    type Guess = Smaller of int | Equal of int | Larger of int 
    let playGameAsyncQueue (controls: FormControls) =  
        let ev = AsyncEventQueue()        
        // To avoid passing 'numToGuess', use mutable binding that 
        // is accessible to all mutually recursive functions.
        // let mutable numToGuess = 0

        let rec init () =               
            controls.PlayAgainButton.Enabled <- false
            for b in controls.GuessButtons 
                do b.Enabled <- true

            controls.MyGuessBox.Text <- "30"
            controls.AnsBox.Text <- ""        

            async {
                let numToGuess = rand.Next(60) 
                return! waitForGuess numToGuess
            }
        and smallerGuessOutcome guessedNum numToGuess = 
            async{
                let answer = if guessedNum > numToGuess then "yes" else "no"
                controls.AnsBox.Text <- answer            

                return! waitForGuess numToGuess
            }     
        and equalGuessOutcome guessedNum numToGuess = 
            async{
                if guessedNum = numToGuess 
                then 
                    controls.AnsBox.Text <- "You guessed it!"
                    return! endGame () 
                else 
                    controls.AnsBox.Text <-"no"
                    return! waitForGuess numToGuess            
            }     
        and largerGuessOutcome guessedNum numToGuess = 
            async{
                let answer = if guessedNum < numToGuess then " Yes" else "no"
                controls.AnsBox.Text <- answer            

                return! waitForGuess numToGuess
            }   
        and showOutcome guess numToGuess =   
            async {
                match guess with 
                | Smaller n -> return! smallerGuessOutcome n numToGuess
                | Equal n -> return! equalGuessOutcome n numToGuess
                | Larger n -> return! largerGuessOutcome n numToGuess
            }
        and waitForGuess numToGuess = 
            async {
                let! guess = ev.Receive() 
                return! showOutcome guess numToGuess
            }     
        and endGame () = 
            async{
                disable controls.GuessButtons
                controls.PlayAgainButton.Enabled <-true
            }     

        setupForm 
            controls
            [
                controls.IsSmallerButton, (fun _ -> ev.Post (Smaller (int controls.MyGuessBox.Text)));
                controls.IsEqualButton, (fun _ -> ev.Post (Equal (int controls.MyGuessBox.Text)));
                controls.IsLargerButton, (fun _ -> ev.Post (Larger (int controls.MyGuessBox.Text)));
                controls.PlayAgainButton, (fun _ -> Async.StartImmediate (init()))
            ]

            (fun () -> 
                Async.StartImmediate (init())
                controls.Window.Show()
            )

// Provide solution that does not use mutual recursion; async and AsyncQueue

    let playGame (controls: FormControls) = 
        let mutable numToGuess = 0        

        let init () = 
            controls.PlayAgainButton.Enabled <- false
            for b in controls.GuessButtons 
                do b.Enabled <- true

            controls.MyGuessBox.Text <- "30"
            controls.AnsBox.Text <- ""       
            numToGuess <- rand.Next(60)
            
        let endGame () =         
            disable controls.GuessButtons
            controls.PlayAgainButton.Enabled <-true
            
        let onIsSmallerGuess _ =             
            let myGuess = int controls.MyGuessBox.Text            
            if numToGuess < myGuess
            then 
                controls.AnsBox.Text <- "yes"
            else             
                controls.AnsBox.Text <- "no"
        
        let onIsEqualGuess _ = 
            let myGuess = int controls.MyGuessBox.Text
            if numToGuess = myGuess
            then 
                controls.AnsBox.Text <- "You guessed it!"
                endGame()                
            else             
                controls.AnsBox.Text <- "no"

        let onIsLargerGuess _ = 
            let myGuess = int controls.MyGuessBox.Text
            if numToGuess > myGuess
            then 
                controls.AnsBox.Text <- "Yes"                
            else             
                controls.AnsBox.Text <- "no"                        

        setupForm 
            controls

            [
                (controls.IsSmallerButton, onIsSmallerGuess);
                (controls.IsEqualButton, onIsEqualGuess);
                (controls.IsLargerButton, onIsLargerGuess);
                (controls.PlayAgainButton, (fun _  -> init()))
            ]

            (fun () -> 
                init()
                controls.Window.Show()
            )

    playGameAsyncQueue(FormControls())
    playGame(FormControls())    
