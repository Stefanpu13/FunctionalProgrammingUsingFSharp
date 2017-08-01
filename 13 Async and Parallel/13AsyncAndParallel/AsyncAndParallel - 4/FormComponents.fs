namespace Exersices4

open System.Windows.Forms
open System.Drawing

module FormComponents = 

    type FormControls () =  
        member val Window = new Form(Text="Web Source Length", Size=Size(925,225))
        member val MyGuessBox = new TextBox(Location=Point(50,25),Size=Size(400,25))

        member val AnsBox = new TextBox(Location=Point(150,150),Size=Size(200,25))
        member val IsSmallerButton =   
            new Button(Location=Point(50,65),MinimumSize=Size(150,50),
                MaximumSize=Size(100,50),Text="Is the number < ")
        member val IsEqualButton =
            new Button(Location=Point(250,65),MinimumSize=Size(150,50),
                MaximumSize=Size(100,50),Text="Is the number = ")
        member val IsLargerButton  =
            new Button(Location=Point(450,65),MinimumSize=Size(150,50),
                MaximumSize=Size(100,50),Text="Is the number > ")
        member val PlayAgainButton  =
            new Button(Location=Point(700,65),MinimumSize=Size(150,50),
                            MaximumSize=Size(100,50),Text="Play again?")                    

        // Does this return new list every time it is accessed? No!
        member t.GuessButtons = [t.IsSmallerButton;t.IsEqualButton;t.IsLargerButton]

    let setupForm (controls: FormControls) buttonsClickHandlers startFunc =             
        let (allControls:Control list) = 
            [
                controls.MyGuessBox;
                controls.AnsBox;
                controls.IsSmallerButton;
                controls.IsEqualButton;
                controls.IsLargerButton; 
                controls.PlayAgainButton] 

        List.iter (fun control -> printfn "control: %A" control; controls.Window.Controls.Add control) allControls

        List.iter (fun (control: Button, handler) -> control.Click.Add handler) buttonsClickHandlers

        startFunc()
    