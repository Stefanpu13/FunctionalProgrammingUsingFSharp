namespace Exercises5
open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing
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
    type Country = string

    type BinTree<'a> = 
        |Leaf of Country
        | Node of BinTree<'a> * 'a * BinTree<'a>

    // build the windows    
    let window =
        new Form(Text="Web Source Length", Size=Size(800,300))
    let questionBox =
        new TextBox(Location=Point(100,25),Size=Size(600,25), ReadOnly=true)
    
    let yesButton =
        new Button(Location=Point(150,75), MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Yes.")
    let noButton =
        new Button(Location=Point(500,75),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="No.")

    let answerBox =
        new TextBox(Location=Point(200,175),Size=Size(400,25), ReadOnly=true)        
    let isLargerButton =
        new Button(Location=Point(450,65),MinimumSize=Size(150,50),
            MaximumSize=Size(100,50),Text="Is the number > ")


    // build tree from questions, associated with european countries
    (*
        From given dataset containg questions build a valid bin tree to play the game
        What is a valid tree?
        A tree where each leave is uniquely determined
        1. Each Question has 2 lists - countries with answer 'yes' and countries with answer 'no'
        2.  
    *)

    let quest = 
        Node( 
            Node(
                Leaf "Cyprus",
                "Is this country an island?",
                Leaf "France"
            ),
            "Is this country in EU?",
            Node(
                Leaf "Turkey",
                "Is this country a muslim coountry?",
                Leaf "Russia"
            )
        )

    // Add controls to window
    window.Controls.Add questionBox
    window.Controls.Add yesButton
    window.Controls.Add noButton
    window.Controls.Add answerBox

    window.Show()        