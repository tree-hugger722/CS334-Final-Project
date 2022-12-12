open Combinator
open Parser
open DataReader
open ListGenerator
open Tests
open Evaluator


[<EntryPoint>]

let main args = 

// //ensures correct input
//     if args.Length < 1 then
//         printfn "Usage: dotnet run '<season> <dish>' \n"
//         printfn "Season options: fall, winter, spring, summer"
//         printfn "Dish options: salad (so far! stay tuned for our expanding dish options)"
//         exit 1
// //parse input
//    // let exprn = args.[0] + " " + args.[1]
//     let exprn = args.[0] 
//     let expression = pattribute (prepare exprn)

//     printf "expression: %A \n" expression

    // let (included,excluded) = processExceptions (HardCore (
    //     (SoftCore (Include, [StrName "Romaine Lettuce";Cat Nut])),
    //     (SoftCore (Exclude, [StrName "Butternut Squash"])))
    //     )
    // printf "Included: %A\n" included
    // printf "Excluded: %A\n" excluded

    let toInclude:Ingredient list = []
    let toExclude = [{ Name = "Carrot";Quantity = 4.000M;Unit = Ounce;Season_List = [Summer; Winter; Fall];Category = Vegetable };{ Name = "Butternut Squash";Quantity = 5.000M;Unit = Ounce;Season_List = [Winter; Fall];Category = Vegetable };{ Name = "Beet";Quantity = 4.000M;Unit = Ounce;Season_List = [Summer; Winter; Fall];Category = Vegetable }]
    printf "%s" (prettyprint (ingredientGen (Vegetable) (Winter) (ingr_list) (toInclude) (toExclude)))


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