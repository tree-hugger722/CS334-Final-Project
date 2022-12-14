open Combinator
open Parser
open DataReader
open ListGenerator
open Tests
open Evaluator


[<EntryPoint>]

let main args = 

//ensures correct input
(*
    if args.Length < 1 then
        printfn "Usage: dotnet run '<temperature> <season> <dish> <exceptions>' \n"
        printfn "Season options (pick one): fall, winter, spring, summer"
        printfn "Temperature options (pick one): warm, cold"
        printfn "Exceptions should be formatted as such: 'with Spinach, Beet and without Radish' "
        printfn "Dish options: salad (so far! stay tuned for our expanding dish options)"
        printfn "if you would like to exclude a specific ingredient, please put the ingredient name in quotes"
        exit 1
*)
//parse input
   // let exprn = args.[0] + " " + args.[1]
   
    let exprn = args.[0] 
    //let expression = parse (exprn)
    let a = pname (prepare exprn)
    printf "%A\n" a

    // printf "expression: %A \n" expression
    // let except = HardCore(SoftCore(Include,[StrName "Red Onion"]),SoftCore(Exclude,[StrName "Butternut Squash"]))
    // let saladIngrs = saladGen Fall except
    // printf "%s" (prettyprint saladIngrs)

    // let toInclude:Ingredient list = []
    // let toExclude = [{ Name = "Carrot";Quantity = 4.000M;Unit = Ounce;Season_List = [Summer; Winter; Fall];Category = Vegetable };{ Name = "Butternut Squash";Quantity = 5.000M;Unit = Ounce;Season_List = [Winter; Fall];Category = Vegetable };{ Name = "Beet";Quantity = 4.000M;Unit = Ounce;Season_List = [Summer; Winter; Fall];Category = Vegetable }]
    // printf "%s" (prettyprint [(ingredientGen (Vegetable) (Winter) (ingr_list) (toInclude) (toExclude))])

    // if parse successful, evaluate input

    // let expression = Some(Recipe(AttributeOne(Warm),Dish(Fall,Salad,except)))


    //let a = eval expression
    0