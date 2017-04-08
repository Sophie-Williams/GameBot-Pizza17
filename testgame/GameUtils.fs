open System
open Common.Utils
open Common.Utils.Net
open FSharpx.Collections

 module GameUtils = 
    open Net
    open Konsole

    let private host     = "130.211.89.1";//"test.natodia.net"
    let private port     = 10000
    let private username = "team46"
    let private pass     = "7014a5f0ce"

    exception UnexpectedMessageException of string

    let login () =
        let stream = connect host port 
   
        match readTokens stream with 
        | ["LOGIN"] -> 
            writeLine stream username 
            
            match readTokens stream with 
            | ["PASS"] -> 
                writeLine stream pass
                match readTokens stream with
                | ["OK"] -> Some stream
                | m -> printfn "unexpected msg1 %A" m; None
            | m -> printfn "unexpected msg2 %A" m; None
        | m -> printfn "unexpected msg3 %A" m; None

    let unExp gotMsg expMsg =
        sprintf "Received unexpected message: %s, while expecting: %s" gotMsg expMsg
        |> UnexpectedMessageException |> raise

    let writeLineOk s msg =
        Net.writeLine s msg
        match readTokens s with 
        | "OK"::tokens -> tokens 
        | tokens -> 
            unExp <| sprintf "First token isn't OK, tokens: %A" tokens <| "OK"

    let writeLineOkFlat s msg =
        writeLineOk s msg
        |> String.concat " "

    let wait s =
        assert (writeLineOkFlat s "WAIT" = "")

        match readFlat s with
        | "OK" -> ()
        | m -> unExp m "OK"

    let turnsLeft s =
        writeLineOkFlat s "TURNS_LEFT" |> int

    //let statusWindow = Window (0, 0, 100, 3, ConsoleColor.Black, ConsoleColor.Magenta)

    //let logsWindow  = 
    //    Console.CursorVisible <- false
    //    Window (0, 3, 100, 20, ConsoleColor.Gray, ConsoleColor.Blue)
        
    //let printStatus s =
    //    statusWindow.PrintAt(0, 0, turnsLeft s |> sprintf "\rTurns left: %d")

    let log (msg : string) = 
        ();//logsWindow.WriteLine ("{0}", msg)

    let logPass msg =
        log msg
        msg