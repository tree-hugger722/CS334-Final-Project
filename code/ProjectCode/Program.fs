open Combinator
open Parser
open DataReader
open ListGenerator
open Tests
open Evaluator
open Rules
open System




[<EntryPoint>]

let main args = 

    printfn "\n"
    printfn "Welcome to the Tosser - seasonal salad recipe generator of your dreams! \n"
    printfn "Please input a recipe or press the 'r' key to read the rules! "


    let input = Console.ReadLine()
    

    if input = "r" then 
        let a = printRules 
        printfn "\n"
        printfn "Please input a recipe: "
        let new_input = Console.ReadLine()
        printfn "Here is your recipe for a %s" new_input
        let b = eval (parse new_input)
        printf "\n"

    else
        printfn "\n"
        printfn "Here is your recipe for: %s" input
        let a = eval (parse input)
        printf "\n"


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



    0