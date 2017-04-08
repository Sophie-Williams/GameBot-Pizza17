module TestGame

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

 open GameUtils

 module Maze =
    open Konsole
    open System.IO
    open YoLo.Option.Operators

    type Pos = { 
            x : int
            y : int 
        }
        with 
            static member create x y =
                { x = x; y = y}

            //static member create (x, y) =
            //    {x = x; y = y}
    
            static member (+) ((lhs : Pos), (rhs : Pos)) =
                {  
                    x = lhs.x + rhs.x
                    y = lhs.y + rhs.y
                }

            static member (-) ((lhs : Pos), (rhs : Pos)) =
                {  
                    x = lhs.x - rhs.x
                    y = lhs.y - rhs.y
                }

    //let mapWindow  = 
    //    Window (0, 20, 100, 20, ConsoleColor.White, ConsoleColor.Black)
        
    let scan s cPos graph =
        match writeLineOk s "SCAN" with 
        | n::tokens ->
            let rec go graph =
                function 
                | [] -> graph
                | kind::dx::dy::rest ->
                    cPos + (Pos.create (int dx) (int dy))
                    |> Map.add <| kind <| graph
                    |> go <| rest
                | e -> failwith <| sprintf "unexpected lst %A" e
            go graph tokens
        | [] -> failwith "empty response"
        
    let drawGraph center graph =
        //mapWindow.Clear ()
        Console.CursorVisible <- false
        Console.SetCursorPosition (0, 0)
        let trans = 
            function 
            | "F" -> "."
            | "W" -> "#"
            | "E" -> "X"
            | c   -> c
        
        Console.Clear ()
        Map.iter (fun {x = x; y = y} (k : string) -> 
            Console.SetCursorPosition (30 + x - center.x, 30 + y - center.y)
            printf "%s" <| trans k)
            //mapWindow.PrintAt(mapWindow.WindowWidth / 2 + x - center.x, 
            //                  mapWindow.WindowHeight / 2 + y - center.y, trans k)) 
            graph

        //mapWindow.PrintAtColor (
        //    ConsoleColor.DarkMagenta, 
        //    mapWindow.WindowWidth / 2,
        //    mapWindow.WindowHeight / 2, "O")

    let findAnyPath center graph =
        let neighs pos = 
            [(0, 1); (1, 0); (-1, 0); (0, -1)]
            |> List.map (uncurry Pos.create)
            |> List.map ((+) pos)
            
        let bfs pos path =
            let mutable queue = Queue.conj pos Queue.empty
            let mutable prev  = Map.add pos [] Map.empty
            let mutable path  = None

            while Queue.isEmpty queue |> not do
                let u = Queue.head queue
                queue <- Queue.tail queue
                let uPath = Map.find u prev : Pos list

                
                neighs u
                |> List.tryPick (fun v ->
                    let neighKind = Map.tryFind v graph
                    if not <| Map.containsKey v prev &&
                        neighKind <> Some "W"
                    then 
                        if neighKind = None || neighKind = Some "E"
                        then
                            sprintf "%A" neighKind |> log
                            sprintf "cut pos: %d %d" v.x v.y |> log
                            Some <| v::uPath
                        else
                            prev <- Map.add v (v::uPath) prev
                            queue <- Queue.conj v queue
                            None
                    else None)
                >>= (fun p -> path <- Some p; queue <- Queue.empty; None) |> ignore

            path
        bfs center []
                
        //let rec dfs pos path =
        //    if Map.tryFind pos graph = Some "E"
        //    then 
        //        log "Standing on final pos"
        //        None
        //    else
        //        neighs pos
        //        |> List.tryPick (fun p ->
        //            let neighKind = Map.tryFind p graph
        //            if not <| Set.contains p vis &&
        //               neighKind <> Some "W"
        //            then 
        //                if neighKind = None || neighKind = Some "E"
        //                then
        //                    sprintf "%A" neighKind |> log
        //                    sprintf "cut pos: %d %d" p.x p.y |> log
        //                    Some path
        //                else
        //                    vis <- Set.add p vis
        //                    dfs p <| p::path
        //            else None)

        //dfs center []

    let getAnyDir (center : Pos) =
        function 
        | Some path ->
            sprintf "center pos: %d %d" center.x center.y |> log
            List.fold (fun s p -> s + (sprintf "(%d, %d) " p.x p.y)) "path: " path |> log

            List.tryLast path 
            >>= (fun next -> next - center |> Some)
        | None      -> 
            log "Got no path"
            None

 open Maze
 open GameUtils
        
[<EntryPoint>]
let main argv =
    let rand = Random ()

    match login () with 
    | Some stream ->
        log "Logged in"
        let startTurns = turnsLeft stream
            
        let mutable currPos = Pos.create 0 0 
        let mutable graph =  Map.empty

        while true do 
            let t = turnsLeft stream 
            //printStatus stream

            //if Map.tryFind currPos graph = None
            //then graph <- Map.add currPos "F" graph

            graph <- scan stream {x = 0; y = 0} graph
            drawGraph (Pos.create 0 0) graph
            
            match findAnyPath currPos graph |> getAnyDir currPos with
            | Some dir -> 
                sprintf "MOVE %d %d" dir.x dir.y |> logPass
                |> writeLine stream

                let m = readFlat stream
                log m
                match m with 
                | "OK" -> currPos <- currPos + dir
                | _ -> ()
            | None     -> ()
            
            wait stream
    | None -> 
        log "Failed to log in"

    Console.ReadLine () |> ignore
    0
