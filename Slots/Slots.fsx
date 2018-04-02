open System

module Slots =
    type SymbolDef = { Coefficient: float }

    type Symbol =
        | Apple of SymbolDef
        | Banana of SymbolDef
        | Pineapple of SymbolDef
        | Wildcard of SymbolDef

    let sumSymbolDef (s1: SymbolDef, s2: SymbolDef, s3: SymbolDef) =
        s1.Coefficient + s2.Coefficient + s3.Coefficient
    
    let calcWin (symbol1: Symbol, symbol2: Symbol, symbol3: Symbol) =
        match (symbol1, symbol2, symbol3) with
        | (Apple(sd1)|Wildcard(sd1)), (Apple(sd2)|Wildcard(sd2)), (Apple(sd3)|Wildcard(sd3)) 
        | (Banana(sd1)|Wildcard(sd1)), (Banana(sd2)|Wildcard(sd2)), (Banana(sd3)|Wildcard(sd3)) 
        | (Pineapple(sd1)|Pineapple(sd1)), (Pineapple(sd2)|Wildcard(sd2)), (Pineapple(sd3)|Wildcard(sd3)) 
            -> sumSymbolDef (sd1, sd2, sd3)
        | _ -> 0.0
     
    let genSymbol() = 
        let rnd = new System.Random()
        match rnd.Next(0, 99) + 1 with
        | value when value <= 5 -> Apple({ Coefficient = 0.2 })
        | value when value <= 10 -> Banana({ Coefficient = 0.4 })
        | value when value <= 15 -> Pineapple({ Coefficient = 0.6 })
        | _ -> Wildcard({ Coefficient = 0.0 })

    let genReel() = 
        seq { 0 .. 2 } |> Seq.map (fun _ -> (genSymbol(), genSymbol(), genSymbol()))

module Game =
    type State = 
        | Start 
        | Play 
        | Finished

    let insertMoney() = 
        try
            Some (Console.ReadLine() |> Decimal.Parse)
        with
        | _ -> None

    let pullHandle stake balance =
        let score = Slots.genReel() |> Seq.map (fun row -> Slots.calcWin row) |> Seq.sum
        let balance = (balance - stake) + (stake * (decimal)score)
        balance

    let rec gameLoop state balance =
        match state with
        | Start ->
            printfn "Insert an init balance"
            let initBalance = insertMoney ()
            match initBalance with
            | Some (ib) -> gameLoop Play ib
            | None -> 
                printfn "Invalid amount, try again"
                gameLoop Start balance
        | Play ->
            printfn "Balance is now %f, insert stake" balance
            let stake = insertMoney ()
            match stake with
            | Some (s) -> 
               let newBalance = pullHandle s balance
               match newBalance with
               | nb when nb > 0m -> gameLoop Play nb
               | _ -> gameLoop Finished 0m
            | None -> 
                printfn "Invalid amount, try again"
                gameLoop Play balance
        | Finished ->
            printfn "No balance left"

    let init() = gameLoop Start 0m |> ignore

[<EntryPoint>]
let main argv = 
    Game.init ()
    0