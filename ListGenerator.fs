module ListGenerator
open Parser
open DataReader

(*
    Code to generate lists helpful for outputting particular recipes
*)

// Given a list of ingredients and a season, outputs all ingredients that belong to that season
let getSeasonalIngredients (ingredients: Ingredient list) (season: Season)=
    ingredients |> List.filter(fun ingr -> (ingr.Season_List |> List.contains(season)))

// List containing all ingredients
let ingr_list = formatInput

// Pre-generated lists of ingredients for each season
let fall_ingredients = getSeasonalIngredients (ingr_list) (Fall)
let winter_ingredients = getSeasonalIngredients (ingr_list) (Winter)
let summer_ingredients = getSeasonalIngredients (ingr_list) (Summer)
let spring_ingredients = getSeasonalIngredients (ingr_list) (Spring)