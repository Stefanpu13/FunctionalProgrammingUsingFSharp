namespace Exersices6
open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing

#if INTERACTIVE
#load "Async.fs"
#endif
open Async

module E = 
    let xorb nums = 
        if Array.length nums < 2
        then failwith "Two little elements"
        else Array.fold (^^^) nums.[0] nums.[1..]

    let takeMatchesFromHeap matches =
        let totalXorb = xorb matches
        if totalXorb <> 0
        then 
            let i = Array.findIndex (fun x -> x ^^^ totalXorb < x ) matches            
            (i, matches.[i] ^^^ totalXorb)
        else
            let i = Array.findIndex (fun x -> x > 0 ) matches
            let remaining = if matches.[i] > 1 then 1 else 0
            (i, remaining)

    let window =
        new Form(Text="Game of NIM", Size=Size(800 ,400))
    let matchesBox =
        new TextBox(Location=Point(300,25),Size=Size(200, 25), ReadOnly=true)

    let matchesBoxes = [|1..10|] |> Array.map (fun i -> 
        new TextBox(Location=Point(50 * i, 125),Size=Size(25, 25), ReadOnly=true)
    )
    let playAgainButton = 
        new Button(Location=Point(300,300),MinimumSize=Size(200,50), Text="Play again?")
    let infoBox =
        new TextBox(Location=Point(200,25),Size=Size(400,25), ReadOnly=true)    

    let toMatches matchBoxes = Array.map (fun (m : TextBox)-> int m.Text) matchBoxes
    let rand = Random()
    let mutable matches = [||]  
    type Player = Human | Computer
    let other = function
    | Human -> Computer
    | Computer -> Human
    
    // State machine functions
    let rec init () =        
        matches <- [1..10] |> List.map (fun _ -> rand.Next(100)) |> Array.ofList

        Array.iter2 (fun m (box: TextBox) -> box.Text <-m.ToString() ) matches matchesBoxes
        playAgainButton.Enabled <- false
        Array.iter (fun (matchesBox: TextBox) -> matchesBox.ResetBackColor()) matchesBoxes
        
        let player = if (rand.Next(2) % 2 = 0) then Human else Computer
        
        match player with
        | Computer -> 
            infoBox.Text <- "Computer first:"
            infoBox.BackColor <- Color.Red
        | Human -> 
            infoBox.Text <- "Human first:"
            infoBox.BackColor <- Color.Blue

        async {
            return! makeMove player
        }
    and makeMove (player) = 
        async {
            let (color, (i, remainingMatches)) = 
                match player with
                | Human -> 
                    (Color.Blue , takeMatchesFromHeap (toMatches matchesBoxes))
                | Computer -> 
                    (Color.Red , takeMatchesFromHeap (toMatches matchesBoxes))
            
            matchesBoxes.[i].Text <- remainingMatches.ToString()              
            matchesBoxes.[i].BackColor <- color

            do! Async.Sleep 1000

            matchesBoxes.[i].ResetBackColor()

            if Array.exists (fun m -> m > 0)(toMatches matchesBoxes) 
            then 
                return! (other player) |> makeMove
            else 
                playAgain i color                
        }
    and playAgain matchesBoxIndex color  =         
        matchesBoxes.[matchesBoxIndex].BackColor <- color
        playAgainButton.Enabled <- true


    // Init screen

    playAgainButton.Click.Add (fun _ -> Async.StartImmediate(init()))
    Array.iter (fun (matchesBox: TextBox) -> 
        matchesBox.Click.Add (fun _ -> matchesBox.ReadOnly <- false) 
    ) matchesBoxes

    // Add controls to window        
    window.Controls.Add playAgainButton    
    window.Controls.Add infoBox
    Array.iter window.Controls.Add matchesBoxes
    Async.StartImmediate( init())
    window.Show()  
    