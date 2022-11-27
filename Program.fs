open Combinator
open Parser
open Evaluator

// Notes from Dan:
// - Start more generally, with an ingredient type
// - Create an ingredient Record (like a class)
//      - Ingredient = Quantity * Unit * Season List * Category
// - Units could be volume and mass, for example
// - At a high level, a recipe is a list of ingredients and a list of steps
// - Lists of ingredients could be inputed through a CSV file (with columns 
// for each of the seasons or category filled with boolean)
// - How to dynamically create an ingredient type from the header of a CSV file:
//      - "type provider"
//      - CSVProvider
// - Look at prices on online Stop n Shop store
// - Newget tool to access third-party APIs
// -CSVHelper library that helps you manipulate CSVs (this could be useful for helping us
// - It would be cool to make a language that can both take in recipes (for its ingredients)
// and generates recipes from scratch

[<EntryPoint>]

let main args = 
    
    //let par = pseq ((pleft (pmany1 pletter) (pws1)) |>> stringify) ((pmany1 pletter) |>> stringify) (fun (a, b) -> (a, b))

    if args.Length < 1 then
        printfn "Usage: dotnet run <season>"
        printfn "Season options: fall, winter, spring, summer"
        exit 1

    let expression = parse args.[0]
    
    match expression with
    | Some ast -> printf "%A\n" ast
    | None -> printfn "Invalid program"

    //let i: Ingredient = Ingredient("lettuce", 0, Cup, [Fall], Green)
    //ingredientPrint i

    //let a = [i; i; i; i; i]
    //prettyprint a

    let ingr_rows = convertRows

    printIngredients ingr_rows

    0