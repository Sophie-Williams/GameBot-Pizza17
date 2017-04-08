module GameUtils  
    open System
    open Common.Utils
    open Common.Utils.Net
    open FSharpx.Collections
    open Net
    open Konsole

    let private host     = "salami.pizza"
    let private port     = 10030
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

    /// Writes line, reads OK and returns tokens list
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
        match writeLineOkFlat s "WAIT" with
        | "" -> 
            match readFlat s with
            | "OK" -> ()
            | m -> unExp m "OK"
        | "OK" ->
            ()
        | m -> printfn "MESSAGE ON WAIT NOT EMPTY %s" m
        
    let turnsLeft s =
        match writeLineOk s "TURNS_LEFT" with 
        | left::isNew -> int left
        | lst -> failwith "unexp"