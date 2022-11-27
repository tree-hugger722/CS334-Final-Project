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
    printIngredients fall_ingredients

    // print all winter ingredients
    printfn "%s" "\n"
    printfn "%s" "\n"
    printfn "%s" "Winter ingredients: \n"
    printIngredients winter_ingredients

    // print all summer ingredients
    printfn "%s" "\n"
    printfn "%s" "\n"
    printfn "%s" "Summer ingredients: \n"
    printIngredients summer_ingredients

    // print all spring ingredients
    printfn "%s" "\n"
    printfn "%s" "\n"
    printfn "%s" "Spring ingredients: \n"
    printIngredients spring_ingredients