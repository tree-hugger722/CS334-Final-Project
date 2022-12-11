module Evaluator
open Parser
open DataReader
open ListGenerator
open DataReader
open System

// // Global list of errors
// let errors: string list = []

// (*
//    helper function randomly sorts list to help get random element
// *)
// let ran (r: Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())

// (*
//     exception processer: takes in an exception type and returns a tuple containing
//     a list of ingredients to include, and a list of ingredients to exclude
// *)
// let rec processExceptions (except: Exception) : (Ingredient list * Ingredient list)=
//     match except with
//     | Softcore(flag, names) -> 
//         // find the corresponding ingredient if it exists
//         match flag with
//         | Include -> (processIngrsToInclude names) * []
//         | Exclude -> [] * (processIngrsToExclude names)
//     | HardCore(except1,except2) -> 
//         let (xs, ys) = processExceptions except1
//         let (as, bs) = processExceptions except2
//         (xs @ as, ys @ bs)


(*
    given the name of an ingredient, returns the Ingredient with that name from
    the entered CSV or None (option) if the ingredient doesn't exist
*)
let findIngredient (name: string) =
    let ingr_list = getIngrByName (ingr_list) (name)
    match ingr_list with
    |Some x -> Some x[0]
    |None -> 
        printf "The following ingredient does not seem to exist in our database:\n"
        printf "%s" name
        printf "\n"
        None


(*
    given a list of Names (strings representing ingredients or categories) to
    exclude in a recipe, returns a list of ingredients to exclude
*)
let rec processIngrsToExclude (nameList: Name list) : (Ingredient list)=
    match nameList with
    | x::xs -> 
        match x with
        | Category(cat) -> 
            let category = getCategoryIngredients (ingr_list) (cat)
            match category with
            | Some ys -> (processIngrsToExclude xs) @ ys
            | None -> (processIngrsToExclude xs)
        | Name(name_str) -> 
            let ingredient = (findIngredient name_str)
            match ingredient with
            | Some x -> x::(processIngrsToExclude xs)
            | None -> 
                processIngrsToExclude xs
        | NoName -> (processIngrsToExclude xs)
    | [] -> []

// (*
//     given a list of Names (strings representing ingredients or categories) to
//     include in a recipe, returns a list of ingredients to include
// *)
// let rec processIngrsToInclude (nameList: Name list) : (Ingredient list)=
//     match nameList with
//     | x::xs -> 
//         match x with
//         | Category(cat) -> []
//         | NoName -> (processIngrsToInclude xs)
//         // not sure if the typing on this works
//         | name_str of string -> 
//             if (findIngredient name_str) != None
//                 then findIngredient::(processIngrsToInclude xs) else
//                 else (processIngrsToInclude xs)
//     | x -> 
//         match x with
//         | Category(cat) -> []
//         // not sure if the typing on this works
//         | name_str of string -> 
//             if (findIngredient name_str) != None then findIngredient
//         | NoName -> []
//     | [] -> []


// (*
//     ingredient generator: takes in a food category, season, list of ingredients
//     to include, list of ingredients to exclude, and returns a random ingredient
//     from that season/category from CVS
//     *)
// let rec ingredientGen (foodCat: Category) (season: Season) (includedIngrs: Ingredient list) (excludedIngrs: Ingredient list)= 
//     let ingredientsList = getCategoryIngredients (getSeasonalIngredients (ingr_list) (season)) (foodCat)
//     let chosenIngredient = ingredientsList |> ran (Random ()) |> Seq.head
//     chosenIngredient

// (*
//     salad generator: returns a list of ingredients for salad
//     salad contains (for now): 2 greens (seasonal), one vegetable(seasonal), one dressing
//  *)
// let rec saladGen (season: Season): Ingredient list = 
//     let xs = ingredientGen Green season
//     let ys = ingredientGen Green season
//     //checks to make sure recipe generates two distinct greens
//     if ys.Name = xs.Name then
//         saladGen season
//     else
//         [xs; ys; ingredientGen Vegetable season; ingredientGen Dressing season;]

// (*
//     helper method that prints out a single ingredient in recipe format
//     type Ingredient = {Name:string; Quantity:decimal; Unit:Unit; Season_List: Season List; Category: Category}
//  *)
// let ingredientPrint (i: Ingredient) = 
//     if i.Unit = Bunch then 
//         printf "%A %Aes of %s\n" i.Quantity i.Unit i.Name
//     else
//         printf "%A %As of %s\n" i.Quantity i.Unit i.Name

// let rec printErrors errorList =
//     match errorList with
//     | x::xs -> 
//         printf "%s" x
//         printErrors xs

// (*
//     prints out a list of ingredients in recipe format
//  *)
// let prettyprint (i: Ingredient list) = 
//     printf "Your recipe is: \n"
//     let rec pp i =
//         match i with 
//         |[] -> printf
//         |x::xs -> 
//             ingredientPrint x
//             pp xs 
//     pp i
//     printErrors errors
