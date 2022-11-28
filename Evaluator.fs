module Evaluator
open Parser
open DataReader
open ListGenerator
open DataReader
open System


(*
   helper function randomly sorts list to help get random element
 *)
let ran (r: Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())

(*
    ingredient generator: takes in a food category + season and returns a random ingredient from that season/category from CVS
 *)
let rec ingredientGen (foodCat: Category) (season: Season) = 
    let ingredientsList = getCategoryIngredients (getSeasonalIngredients (ingr_list) (season)) (foodCat)
    let chosenIngredient = ingredientsList |> ran (Random ()) |> Seq.head 
    // if a.Category = foodCat then
    //     a 
    // else
    //     ingredientGen (foodCat) (season)
    chosenIngredient

(*
    salad generator: returns a list of ingredients for salad
    salad contains (for now): 2 greens (seasonal), one vegetable(seasonal), one dressing
 *)

let rec saladGen (season: Season): Ingredient list = 
    let xs = ingredientGen Green season
    let ys = ingredientGen Green season
    //checks to make sure recipe generates two distinct greens
    if ys.Name = xs.Name then
        saladGen season
    else
        [xs; ys; ingredientGen Vegetable season; ingredientGen Dressing season;]

(*
    helper method that prints out a single ingredient in recipe format
    type Ingredient = {Name:string; Quantity:decimal; Unit:Unit; Season_List: Season List; Category: Category}
 *)
let ingredientPrint (i: Ingredient) = 
    if i.Unit = Bunch then 
        printf "%A %Aes of %s\n" i.Quantity i.Unit i.Name
    else
        printf "%A %As of %s\n" i.Quantity i.Unit i.Name

(*
    prints out a list of ingredients in recipe format
 *)
let prettyprint (i: Ingredient list) = 
    printf "Your recipe is: \n"
    let rec pp i =
        match i with 
        |[] -> printf
        |x::xs -> 
            ingredientPrint x
            pp xs 
    pp i
