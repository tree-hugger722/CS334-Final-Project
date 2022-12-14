module Tests
open DataReader
open ListGenerator
open Evaluator

(* 
    Code containing helpful print tests
*)



// Prints all ingredients
let testIngredientGeneration = 
    printIngredients ingr_list
    printfn "%s" "\n"
    printfn "%s" "\n"

let testSeasonalIngredientLists =
    // print all fall ingredients
    printfn "%s" "Fall ingredients: \n"
    match fall_ingredients with
    |Some x -> printIngredients x
    |None -> printf "No ingredients belong to the fall season"


    // print all winter ingredients
    printfn "%s" "\n"
    printfn "%s" "\n"
    printfn "%s" "Winter ingredients: \n"
    match winter_ingredients with
    |Some x -> printIngredients x
    |None -> printf "No ingredients belong to the winter season"

    // print all summer ingredients
    printfn "%s" "\n"
    printfn "%s" "\n"
    printfn "%s" "Summer ingredients: \n"
    match summer_ingredients with
    |Some x -> printIngredients x
    |None -> printf "No ingredients belong to the summer season"

    // print all spring ingredients
    printfn "%s" "\n"
    printfn "%s" "\n"
    printfn "%s" "Spring ingredients: \n"
    match spring_ingredients with
    |Some x -> printIngredients x
    |None -> printf "No ingredients belong to the spring season"
