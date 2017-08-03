namespace Exercises5
open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing

#if INTERACTIVE
#load "QuestTree.fs"
#load "../Async.fs"

// Open namespace, so other files/modules can be opened
open Exercises5
#endif
open Quest
open Async

module E = 
    (* 5.
        Make a geography program guessing a country in Europe. The program asks questions to the
        user who answers yes or
         no. The program should use a binary tree with country names in the
        leaves and with a question in each node, such that the left subtree is chosen in case of answer
        yes and the right in case of answer no.
        The program can be made to look more “intelligent” by inserting some random questions in
        between the systematic questions taken from the tree. The random questions should be of two
        kinds: Silly questions where the answer is not used by the program, and direct questions guessing
        a specific country where the answer is used by the program in case it gets answer yes.
    *)
    // build the windows    
    let window =
        new Form(Text="Web Source Length", Size=Size(800,400))
    let questionBox =
        new TextBox(Location=Point(100,25),Size=Size(600,25), ReadOnly=true)
    
    let yesButton =
        new Button(Location=Point(150,75), MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Yes.")
    let noButton =
        new Button(Location=Point(500,75),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="No.")

    let playAgainButton = 
        new Button(Location=Point(325,225),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Play again?")
    let answerBox =
        new TextBox(Location=Point(200,175),Size=Size(400,25), ReadOnly=true)        
    let isLargerButton =
        new Button(Location=Point(450,65),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Is the number > ")


    type Answer = Yes | No
    let ev = Async.AsyncEventQueue()
    
    let toggle bs state = 
        for (b:Button) in bs do
            b.Enabled <- state      

    let disable bs = toggle bs false
    let enable bs = toggle bs true      

    let rec playGame quest = 
        match quest with
            | Leaf c -> 
                async {
                    disable [yesButton;noButton]      
                    enable [playAgainButton]              
                    answerBox.Text <- sprintf "your country is %s" c
                }                
            | Node (lt, q, rt) ->                 
                async {                    
                    questionBox.Text <- q
                    let! resp = ev.Receive()
                    match resp with
                    | Yes ->  return! playGame lt
                    | No -> return! playGame rt
                }
    let init () = 
        enable [yesButton;noButton]  
        disable [playAgainButton]
        let quest = getQuestTree()
        playGame quest

    yesButton.Click.Add (fun _ -> ev.Post Yes)
    noButton.Click.Add (fun _ -> ev.Post No)
    playAgainButton.Click.Add (fun _ -> Async.StartImmediate(init()))

    // Add controls to window    
    window.Controls.Add yesButton
    window.Controls.Add noButton
    window.Controls.Add playAgainButton
    window.Controls.Add questionBox
    window.Controls.Add answerBox

    Async.StartImmediate( init())
    window.Show()        
    