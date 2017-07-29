module Async

open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing

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
    new Form(Text="Web Source Length", Size=Size(525,225))
let urlBox =
    new TextBox(Location=Point(50,25),Size=Size(400,25))
let ansBox = new TextBox(Location=Point(150,150),Size=Size(200,25))
let startButton =
    new Button(Location=Point(50,65),MinimumSize=Size(100,50),
        MaximumSize=Size(100,50),Text="START")
let clearButton =
    new Button(Location=Point(200,65),MinimumSize=Size(100,50),
        MaximumSize=Size(100,50),Text="CLEAR")
let cancelButton =
    new Button(Location=Point(350,65),MinimumSize=Size(100,50),
        MaximumSize=Size(100,50),Text="CANCEL")

let disable bs = 
    for b in [startButton;clearButton;cancelButton] do
        b.Enabled <- true
    for (b:Button) in bs do
        b.Enabled <- false

// The dialogue part from Table 13.7 belongs here
type Message =  
    | Start of string | Clear | Cancel 
    | Web of string | Error | Cancelled
let ev = AsyncEventQueue()

let rec ready() =
    async {  // actionReady: actions for incoming events
        urlBox.Text <- "http://"
        ansBox.Text <- ""

        disable [cancelButton]
        
        let! msg = ev.Receive()           
        printfn "received in 'ready': %A" msg

        match msg with
        | Start url -> return! loading(url)
        | Clear -> return! ready()
        | _ -> failwith("ready: unexpected message")
    }
and loading(url) =
    async { // actionLoading: actions for incoming events
        ansBox.Text <- "Downloading"
        use ts = new CancellationTokenSource()
        Async.StartWithContinuations
            (
                async { 
                    let webCl = new WebClient()
                    let! html = webCl.AsyncDownloadString(Uri url)
                    return html 
                    },
                (fun html -> 
                    printfn "2. post method is called in continuation?"
                    ev.Post (Web html)),
                (fun _ -> 
                    printfn "2. post method is called in continuation?"
                    ev.Post Error),
                (fun _ ->
                    printfn "2. post method is called in continuation?" 
                    ev.Post Cancelled),
                ts.Token)

        disable [startButton; clearButton]

        let! msg = ev.Receive()
        match msg with
        | Web html ->
            let ans = sprintf "Length = %i" html.Length//String.Format("0:D",html.Length)
            return! finished(ans)
        | Error -> return! finished("Error")
        | Cancel -> 
            ts.Cancel()
            return! cancelling()
        | _ -> failwith("loading: unexpected message")
    }
and cancelling() =
    async { // actionCancelling: actions for incoming events
        ansBox.Text <- "Cancelling"

        disable [startButton; clearButton; cancelButton]

        let! msg = ev.Receive()
        match msg with
        | Cancelled | Error
        | Web _ -> return! finished("Cancelled")
        | _ -> failwith("cancelling: unexpected message")
    }
and finished(s) =
    async { // actionFinished: actions for incoming events        
        ansBox.Text <- s

        disable [startButton; cancelButton]

        let! msg = ev.Receive()
        match msg with
        | Clear -> return! ready()
        | _ -> failwith("finished: unexpected message")
    }
// Initialization
window.Controls.Add urlBox
window.Controls.Add ansBox
window.Controls.Add startButton
window.Controls.Add clearButton
window.Controls.Add cancelButton
startButton.Click.Add (fun _ -> ev.Post (Start urlBox.Text))
clearButton.Click.Add (fun _ -> ev.Post Clear)
cancelButton.Click.Add (fun _ -> ev.Post Cancel)
// Start
Async.StartImmediate (ready())
window.Show()

// ------------ Dialog program end -----------

let async3 = 
    async {
        return 3
    }


Async.StartWithContinuations(
    async{
        return 1
    },
    (fun el -> printfn "cont:%A" el),
    (fun el -> printfn "err:%A" el),
    (fun el -> printfn "canc:%A" el)
)

let ready2 = 
    async {
        printfn "runs to here"
        let res =         
            Async.FromContinuations (fun (cont,econt,ccont) ->
                cont())

        printfn "never gets here: %A" res
        // res
    }

Async.StartImmediate(ready2)
