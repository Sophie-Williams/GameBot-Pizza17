module Game

open System
open Common.Utils
open Common.Utils.Net
open FSharpx.Collections
open GameUtils

type Pos = { 
        x : float
        y : float
    }
    with 
        static member create x y =
            { x = x; y = y}
    
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

        static member sqrDist lhs rhs =
            (rhs.x - lhs.x) * (rhs.x - lhs.x) + (rhs.y - lhs.y) * (rhs.y - lhs.y)

type Constants = {
        n                : int
        startWoolCount   : int
        maxCapacity      : int
        radius           : float
        sheepSpeed       : float
        sheepNearSpeed   : float
        sheepWarnedSpeed : float
        sheppardSpeed    : float
        dogsSpeed        : float
        sheepsDist       : float
        turnsDogCanBark  : int
        barkingDist      : float
        calmingDownTurns : int
    }

type Owner = A | B | Neutral

type Player = IAmA | IAmB

type FieldType =
    | Locked
    | Free
    | Sheed of Owner

type Sheep = {
        id      : int
        pos     : Pos
        wool    : int
        sheared : bool
    }

type Dog = { pos : Pos; id : int }

type Sheppard = {
        id           : int
        pos          : Pos
        shearedSheep : int option
        carriedWool  : int
    }

type EnemySheppard = { pos : Pos; id : int }

type Sheed = {
        row         : int
        col         : int 
        player      : Owner
        wool        : int
        playerADogs : int
        playerBDogs : int 
    }

type Units = {
        player         : Player
        myDogs         : Dog list
        mySheppards    : Sheppard list

        enemyDogs      : Dog list
        enemySheppards : EnemySheppard list
    }
    with static member create player = {
            player         = player
            myDogs         = []
            mySheppards    = []
            enemyDogs      = []
            enemySheppards = []
        }
        
type GameState = {
        constants : Constants
        map       : Map<int * int, FieldType>
        sheeps    : Sheep list
        sheds     : Sheed list
        units     : Units
    }
    
module Commands =
    open System.Net.Sockets
    
    let getConstants (s : NetworkStream) =
        match writeLineOk s "GET_CONSTANTS" with
        | n::swc::maxC::r::sSpeed::snSpeed
          ::swSpeed::sheSpeed::dSpeed::sDist
          ::tdcb::bDist::calmTurns::[] ->

            {
                n                = int n
                startWoolCount   = int swc
                maxCapacity      = int maxC
                radius           = float r
                sheepSpeed       = float sSpeed
                sheepNearSpeed   = float snSpeed
                sheepWarnedSpeed = float swSpeed
                sheppardSpeed    = float sheSpeed
                dogsSpeed        = float dSpeed
                sheepsDist       = float sheSpeed
                turnsDogCanBark  = int tdcb
                barkingDist      = float bDist
                calmingDownTurns = int calmTurns
            } 
        | lst -> 
            failwith <| sprintf "unexpected tokens: %A" lst
            
    let getMap (s : NetworkStream) (constants : Constants) =
        let mutable map = Map.empty

        let field =
             function 
                | '#' -> Locked 
                | '.' -> Free
                | 'A' -> Sheed A
                | 'B' -> Sheed B
                | 'S' -> Sheed Neutral
                | c   -> failwith <| sprintf "unexpected char %c" c
        let addToMap i j x = 
            map <- Map.add (i, j) x map

        List.iteri (fun i line ->
            String.iteri 
                (fun j -> field >> (addToMap i j)) line
            ) <| writeLineOk s "GET_MAP"

        map

    let getSheeps (s : NetworkStream) =
        let tCnt::tokens = writeLineOk s "GET_SHEEP"
        let tCnt = int tCnt

        //printfn "tcnt: %d, tokens len: %d" tCnt <| List.length tokens
        //printfn "get sheep tokens: "
        //tokens |> iterPrintfn

        let parseSheep id (x::y::w::u::lst) = 
            {
                id      = id
                pos     = Pos.create <| float x <| float y
                wool    = int w
                sheared = u = "1"
            }, lst
            
        let rec takeMatch sheepsPerMatch tokens = 
            function 
            | 0 -> sheepsPerMatch
            | mCnt ->
                let sCnt::tokens = tokens
                let sCnt = int sCnt
                
                let rec takeSheeps sheeps tokens idCnt =
                    function 
                    | 0 -> sheeps, tokens
                    | sCnt ->
                        let s, tokens = parseSheep idCnt tokens
                        //printfn "prev scnt %d" <| sCnt
                        takeSheeps (s::sheeps) tokens (idCnt + 1) <| sCnt - 1

                let sheeps, tokens = takeSheeps [] tokens 1 sCnt
                takeMatch (sheeps::sheepsPerMatch) tokens <| mCnt - 1

        takeMatch [] tokens tCnt

    let getUnits (s : NetworkStream) =
        let tCnt::tokens = writeLineOk s "GET_UNITS"
        let tCnt = int tCnt

        let parseMySheppard id (x::y::h::w::lst) = 
            {
                id           = id
                pos          = Pos.create <| float x <| float y
                shearedSheep = 
                    //printfn "H: %s" h
                    if h = "0" then None else Some <| int h 
                carriedWool  = int w
            }, lst
        
        let parseEnemySheppard id (x::y::lst) = 
            {
                id = id
                EnemySheppard.pos = Pos.create <| float x <| float y
            }, lst
        
        let parseDog id (x::y::lst) = 
            {
                id = id
                Dog.pos = Pos.create <| float x <| float y
            }, lst
        
        let rec takeMatch unitsPerMatch tokens = 
            function 
            | 0 -> unitsPerMatch
            | mCnt ->
                let player::sheCnt::tokens = tokens
                let sheCnt = int sheCnt
                let player =
                    match player with 
                    | "1" -> IAmA
                    | "2" -> IAmB
                    | s   -> failwithf "unexpected iam player id %s" s 

                let rec takeSth parse sth tokens idCnt =
                    function 
                    | 0 -> sth, tokens
                    | sCnt ->
                        let s, tokens = parse idCnt tokens
                        takeSth parse (s::sth) tokens <| idCnt + 1 <| sCnt - 1 

                let mySheppards, tokens = takeSth parseMySheppard [] tokens 1 sheCnt
                
                let dogCnt::tokens = tokens
                let dogCnt         = int dogCnt
                let myDogs, tokens = takeSth parseDog [] tokens 1 dogCnt
                
                let enSheCnt::tokens       = tokens
                let enSheCnt               = int enSheCnt
                let enemySheppards, tokens = takeSth parseEnemySheppard [] tokens 1 enSheCnt

                let enDogCnt::tokens  = tokens
                let enDogCnt          = int enDogCnt
                let enemyDogs, tokens = takeSth parseDog [] tokens 1 enDogCnt

                {
                    player         = player
                    mySheppards    = mySheppards
                    myDogs         = myDogs
                    enemySheppards = enemySheppards
                    enemyDogs      = enemyDogs
                }
                |> fun units -> takeMatch (units::unitsPerMatch) tokens <| mCnt - 1

        takeMatch [] tokens tCnt

    let getSheds (s : NetworkStream) =
        //writeLine s "GET_SHEDS" 
        //readAll s |> printf "%s"
        //[]
        let tCnt::tokens = writeLineOk s "GET_SHEDS"
        let tCnt = int tCnt

        //printfn "get sheds tokens: "
        //tokens |> iterPrintfn

        let parseSheed (r::c::k::w::da::db::lst) = 
            {
                row = int r
                col = int c
                player =
                    match k with
                    | "1" -> A
                    | "2" -> B
                    | "0" -> Neutral
                    | c   -> failwithf "unexp c: %s" c
                wool = int w
                playerADogs = int da
                playerBDogs = int db
            }, lst
        
        let rec takeMatch shedsPerMatch tokens = 
            function 
            | 0 -> shedsPerMatch
            | mCnt ->
                let sheCnt::tokens = tokens
                let sheCnt = int sheCnt
               
                let rec takeSth parse sth tokens =
                    function 
                    | 0 -> sth, tokens
                    | sCnt ->
                        let s, tokens = parse tokens
                        takeSth parse (s::sth) tokens <| sCnt - 1

                let sheds, tokens = takeSth parseSheed [] tokens sheCnt
                takeMatch (sheds::shedsPerMatch) tokens <| mCnt - 1

        takeMatch [] tokens tCnt
    
    type UnityType = Dog | Sheppard

    let move (s : NetworkStream) (matchNum : int) unit unitNum (vec : Pos) =
        let unit =
            match unit with 
            | Sheppard -> 1
            | Dog      -> 2

        sprintf "MOVE %d %d %d %f %f" 
            matchNum unit unitNum vec.x vec.y 
        |> writeLine s 

        match readFlat s with 
        | "OK" -> true 
        | err  -> 
            printfn "error on move: %s" err
            false

    type Shearing = Start | End
            
    let shear (s : NetworkStream) (matchNum : int) (shear : Shearing) sheppardNum sheepNum =
        let shear =
            match shear with 
            | Start -> 1
            | End   -> 0

        sprintf "SHEAR %d %d %d %d" 
            matchNum shear sheppardNum sheepNum 
        |> writeLine s 

        match readFlat s with 
        | "OK" -> true 
        | err  -> 
            printfn "error on shear: %s" err
            false

    type Transfering = Put | Take

    let transferWool (s : NetworkStream) matchNum (transfer : Transfering) sheppardNum woolCnt =
        let transfer =
            match transfer with 
            | Put  -> 1
            | Take -> 0

        sprintf "TRANSFER_WOOL %d %d %d %d" 
            matchNum transfer sheppardNum woolCnt 
        |> writeLine s 

        match readFlat s with 
        | "OK" -> true 
        | err  -> 
            printfn "error on transfer: %s" err
            false

    let printMap map constants =
        let printField = 
            function 
            | Locked -> '#'
            | Free   -> '.'
            | Sheed A -> 'A'
            | Sheed B -> 'B'
            | Sheed Neutral -> 'S'
           
        for i in 0..constants.n-1 do
            for j in 0..constants.n-1 do 
                printf "%c" <| printField (Map.find (i, j) map)

            printfn ""
