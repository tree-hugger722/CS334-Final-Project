open Combinator
open Parser
open DataReader
open ListGenerator
open Tests
open Evaluator


[<EntryPoint>]

let main args = 

//ensures correct input
    if args.Length < 1 then
        printfn "Usage: dotnet run '<season> <dish>' \n"
        printfn "Season options: fall, winter, spring, summer"
        printfn "Dish options: salad (so far! stay tuned for our expanding dish options)"
        exit 1
//parse input
   // let exprn = args.[0] + " " + args.[1]
    let exprn = args.[0] 
    let expression = parse exprn

    printf "expression: %A \n" expression

(*//if parse successful, evaluate input
    match expression with
    | Some ast -> 
        match ast with 
        |Plate(season, dish) -> 
            if dish = Salad then
                let a = prettyprint (saladGen season) 
                printf "\n"
    | None -> printf "Invalid"
*)
    0