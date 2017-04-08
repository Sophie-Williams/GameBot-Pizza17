module TestGame

open System
open Common.Utils
open Common.Utils.Net
open FSharpx.Collections
open GameUtils
open Game
    
open Commands

let playMatch turns stream (matchNum : int) (state : GameState) =
    let me = state.units.player
    let owned = 
        match me with 
        | IAmA -> A
        | IAmB -> B

    let sheed = List.find (fun (v : Sheed) -> v.player = owned) state.sheds 
    let sheedPos = Pos.create <| float sheed.col <| float sheed.row

    let totalSheps = List.length state.units.mySheppards
    let carringSheppards, notCarringSheppards = 
        state.units.mySheppards 
        |> List.indexed
        |> List.partition (fun (_, sh) -> sh.carriedWool > 0)

    let sheppardsToPut, carringSheppards = 
        List.partition 
            (fun (_, (sh : Sheppard)) -> Pos.sqrDist sheedPos sh.pos < 1.0)
            carringSheppards

    carringSheppards 
    |> List.iter
        (fun (shepId, (shep : Sheppard)) -> 
            move stream matchNum Sheppard shep.id (sheedPos + (Pos.create 0.2 0.2) - shep.pos) |> ignore)

    sheppardsToPut 
    |> List.iter
        (fun (shepId, (shep : Sheppard)) -> 
            let res = transferWool stream matchNum Put shep.id shep.carriedWool
            printfn "TRANSFERED WOOL: %b" res
            if res = false 
            then 
                let (Some sharedSheepId) = shep.shearedSheep
                let sharedSheep = List.find (fun (s : Sheep) -> s.id = sharedSheepId) state.sheeps 
                shear stream matchNum End shep.id sharedSheepId 
                |> printfn "stopped shearing: %b"
            )

    notCarringSheppards
    |> List.iter
        (fun (shepId, (shep : Sheppard)) -> 
            let closSheepId, closestSheep = 
                state.sheeps
                |> List.indexed
                |> List.filter (fun (i, (s : Sheep)) -> not s.sheared)
                |> List.minBy (fun (i, s) -> Pos.sqrDist shep.pos s.pos)

            if Pos.sqrDist closestSheep.pos shep.pos < 4.0 * (state.constants.radius ** 2.0)
            then
                if shep.shearedSheep <> None 
                then
                    let (Some sharedSheepId) = shep.shearedSheep
                    let sharedSheep = List.find (fun (s : Sheep) -> s.id = sharedSheepId) state.sheeps 

                    if sharedSheep.wool = 0 || shep.carriedWool >= state.constants.maxCapacity - 5
                    then 
                        
                        shear stream matchNum End shep.id sharedSheepId 
                        |> printfn "stopped shearing: %b"
                else
                    let res = shear stream matchNum Start shep.id closestSheep.id
                    printfn "started shearing: %b" res

                    if not res then 
                        printfn "sheppered: %A, sheep: %A, dist: %f, radius: %f" 
                            shep.pos closestSheep (Pos.sqrDist shep.pos closestSheep.pos) state.constants.radius
            else
                //printfn "Almost: sheppered: %A, sheep: %A, dist: %f, radius: %f" 
                //            shep.pos closestSheep (Pos.sqrDist shep.pos closestSheep.pos) state.constants.radius
                move stream matchNum Sheppard shep.id (closestSheep.pos - shep.pos) |> ignore)

    if turns % 10 = 0 || turns % 11 = 0 
    then 
        printfn "not carring cnt: %d\nsheppardsToPut: %d\ncarringSheppards: %d"
            (List.length notCarringSheppards) (List.length sheppardsToPut) (List.length carringSheppards)
    ()


let round turns map consts stream = 
    let sheeps = getSheeps stream
    let sheds = getSheds stream
    let units = getUnits stream
    
    let rec makeMatches matches =
        function 
        | [], [], [] -> matches
        | sh::sheeps, she::sheds, us::units ->
            {   
                units     = us
                sheds     = she
                sheeps    = sh
                map       = map
                constants = consts 
            } |> fun m -> makeMatches (m::matches) (sheeps, sheds, units)
        | _ -> failwith "unexp"

    makeMatches [] (sheeps, sheds, units)
    |> List.iteri (fun i -> playMatch turns stream (i + 1))

    ()

[<EntryPoint>]
let main argv =
    let rand = Random ()

    match login () with 
    | Some stream ->
        //getSheeps stream |> iterPrintfn
        let consts = getConstants stream
        let map = getMap stream consts
       
        printMap map consts

        //getSheeps stream |> iterPrintfn
        //getSheds stream |> iterPrintfn
        //getUnits stream |> ignore
        while true do
            let turns = turnsLeft stream
            printfn "turns left %d" <| turns
            round turns map consts stream
            wait stream
    | None -> 
        printfn "Failed to log in"

    Console.ReadLine () |> ignore
    0
