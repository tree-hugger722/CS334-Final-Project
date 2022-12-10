module ListGenerator
open Parser
open DataReader

(*
    Code containing functions to generate lists helpful for outputting particular recipes
    and pre-generated lists
*)

// Global list containing all ingredients
let ingr_list = formatInput

// Given a list of ingredients and a season, outputs all ingredients that belong to that season
let getSeasonalIngredients (ingredients: Ingredient list) (season: Season)=
    ingredients |> List.filter(fun ingr -> (ingr.Season_List |> List.contains(season)))

// Given a list of ingredients and a category, outputs all ingredients that belong to that category
let getCategoryIngredients (ingredients: Ingredient list) (category: Category)=
    ingredients |> List.filter(fun ingr -> (ingr.Category = category))

let greens = getCategoryIngredients (ingr_list) (Green)
let veggies = getCategoryIngredients (ingr_list) (Vegetable)
let dressings = getCategoryIngredients (ingr_list) (Dressing)

// Pre-generated lists of ingredients for each season
let fall_ingredients = getSeasonalIngredients (ingr_list) (Fall)
let winter_ingredients = getSeasonalIngredients (ingr_list) (Winter)
let summer_ingredients = getSeasonalIngredients (ingr_list) (Summer)
let spring_ingredients = getSeasonalIngredients (ingr_list) (Spring)