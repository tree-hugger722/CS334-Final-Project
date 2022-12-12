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


(**************************PARSER EXCEPTION TYPE HANDLING**************************)
(*
    given a list of Names (strings representing ingredients or categories) to
    exclude in a recipe, returns a list of ingredients to exclude
*)
let rec processIngrsToExclude (nameList: Name list) : (Ingredient list)=
    match nameList with
    | x::xs -> 
        match x with
        | Cat(cat) -> 
            let category = getCategoryIngredients (ingr_list) (cat)
            match category with
            | Some ys -> (processIngrsToExclude xs) @ ys
            | None -> (processIngrsToExclude xs)
        | StrName(name_str) -> 
            let ingredient = (findIngredient name_str)
            match ingredient with
            | Some x -> x::(processIngrsToExclude xs)
            | None -> 
                processIngrsToExclude xs
        | NoName -> (processIngrsToExclude xs)
    | [] -> []

(*
    given a list of Names (strings representing ingredients or categories) to
    include in a recipe, returns a list of ingredients to include
*)
let rec processIngrsToInclude (nameList: Name list) : (Ingredient list)=
    match nameList with
    | x::xs -> 
        match x with
        | Cat(cat) -> 
            printf "We unfortunately do not support the inclusion of food categories.\n"
            printf "Our recipe generator has an opinion on which categories of foods should be included in a salad :)\n"
            printf "If you have an ingredient in mind, please try again with the ingredient in the place of the category.\n"
            processIngrsToExclude xs
        | StrName(name_str) -> 
            let ingredient = (findIngredient name_str)
            match ingredient with
            | Some x -> x::(processIngrsToInclude xs)
            | None -> 
                processIngrsToInclude xs
        | NoName -> (processIngrsToInclude xs)
    | [] -> []

(*
    exception processer: takes in an exception type and returns a tuple containing
    a list of ingredients to include, and a list of ingredients to exclude
*)
let rec processExceptions (except: Parser.Exception) : (Ingredient list * Ingredient list)=
    match except with
    | SoftCore(flag, names) -> 
        // find the corresponding ingredient if it exists
        match flag with
        | Include -> (processIngrsToInclude names),[]
        | Exclude -> [],(processIngrsToExclude names)
    | HardCore(except1,except2) -> 
        let (xs, ys) = processExceptions except1
        let (bs, cs) = processExceptions except2
        (xs @ bs, ys @ cs)
    | NoException -> ([],[])


(**************************INGREDIENT GENERATION**************************)
(*
    ingredient getter: takes in a food category, season, and list of ingredients
    to consider, and returns the list of ingredients from that season/category in
    the CVS
*)
let getIngredients (foodCat: Category) (season: Season) (ingredients: Ingredient list) =
    // Get list of ingredients from season
    let seasonalIngrs = (getSeasonalIngredients (ingredients) (season))
    
    // Check that there are actually ingredients from the given season
    match seasonalIngrs with
    | Some x -> 
        // Get list of seasonal ingredients from the given category
        let ingredientsList = getCategoryIngredients (x) (foodCat)
        // Check that are actually ingredients from the given season AND the given category
        match ingredientsList with
        | Some y -> y
        | None -> 
            printf "Invalid category"
            []
    | None ->
        printf "Invalid season"
        []


(*
    random ingredient generator: takes in a food category, season, list of 
    ingredients to exclude, and returns a list with a single random 
    ingredient from that season/category from CVS, or an empty list if no
    ingredient satisfies the requirements
    *)
let randomIngrGen (foodCat: Category) (season: Season) (ingredients: Ingredient list)(excludedIngrs: Ingredient list)= 
    let list = getIngredients (foodCat) (season) (ingredients)

    // Function that takes in an ingredient and checks if it is in the list of
    // ingredients to exclude
    // Returns Some if so, None otherwise
    let excludeChooser (ingr: Ingredient)=
        let isInList = List.contains ingr excludedIngrs
        match isInList with
        |true -> None
        |false -> Some(ingr)
  
    let listWithoutExcludedIngrs = list |> List.choose excludeChooser
    match listWithoutExcludedIngrs with
    |[] -> []
    | _ -> [(listWithoutExcludedIngrs |> ran (Random ()) |> Seq.head)]


(**************************SALAD GENERATION**************************)

(*
    included ingredients map creator: given a list of ingredients to include,
    returns a map<category, ingredient list>
*)
let rec createIncludedIngrsMap (includedIngrs: Ingredient list) =
    match includedIngrs with
    |[] -> Map.empty<Category,Ingredient list>
    |[x] -> (createIncludedIngrsMap []) |> Map.add (x.Category) ([x])
    |x::xs -> 
        let containsIngr = (createIncludedIngrsMap xs) |> Map.containsKey (x.Category)
        if containsIngr = true then
            (createIncludedIngrsMap xs) |> Map.change x.Category (fun y -> 
                match y with
                | Some s -> Some (x::s)
                | None -> None)
        else
            (createIncludedIngrsMap xs) |> Map.add (x.Category) ([x])

// (*
//     ingredient generator: given a category, season, list of ingredients
//     to consider, list of ingredients to exclude, and a dictionary of ingredients
//     to include, returns either a random ingredient (not in the excluded list) if
//     there are no ingredients of the correct category in the include list, or an 
//     appropriate ingredient from the include list
// *)
// let ingredientGen (foodCat:Category) (season:Season) (ingredients:Ingredient list) (excludedIngrs: Ingredient list) (includeDict:Map<Category,Ingredient list>) =
//     if Map.containsKey (foodCat) (includeDict) then
//         [(Map.find (foodCat) (includeDict))[0]]
//     else 
//         randomIngrGen (foodCat) (season) (ingredients) (excludedIngrs)

// (*
//     salad generator: returns a list of ingredients for salad
//     salad contains (for now): 2 greens (seasonal), one vegetable(seasonal), one dressing
//  *)
// let rec saladGen (season: Season) (except: Parser.Exception): Ingredient list = 
//     let (includedIngrs,excludedIngrs)= processExceptions except

//     let includeMap= createIncludedIngrsMap includedIngrs

//     let green = (ingredientGen Green season ingr_list includedIngrs excludedIngrs)

//     let veggie1 = (ingredientGen Vegetable season ingr_list includedIngrs excludedIngrs)

//     let veggie2 = (ingredientGen Vegetable season ingr_list includedIngrs excludedIngrs)

//     let cheese = (ingredientGen Cheese season ingr_list includedIngrs excludedIngrs)

//     let nut = (ingredientGen Nut season ingr_list includedIngrs excludedIngrs)

//     let dressing = (ingredientGen Dressing season ingr_list includedIngrs excludedIngrs)

//     dressing@nut@cheese@veggie2@veggie1@green


(**************************PRINTING**************************)
(*
    helper method that prints out a single ingredient in recipe format
    type Ingredient = {Name:string; Quantity:decimal; Unit:Unit; Season_List: Season List; Category: Category}
 *)
let ingredientPrint (i: Ingredient) = 
    if i.Unit = Bunch then 
        let output = sprintf "%A %Aes of %s \n" i.Quantity i.Unit i.Name
        output
    else if i.Unit = Whole then 
        let output = sprintf "%A %ss \n" i.Quantity i.Name
        output
    else
        let output = sprintf "%A %As of %s \n" i.Quantity i.Unit i.Name
        output


(*
    prints out a list of ingredients in recipe format
 *)
let prettyprint (i: Ingredient list) = 
    let rec pp i =
        match i with 
        |[] -> ""
        |x::xs -> 
            let str = (pp xs) + (ingredientPrint x)
            str
    let finalStr = "Your recipe is: \n" + pp i
    //printf "%s" finalStr
    finalStr

(**************************EVALUATION**************************)

// (*
//     top-level evaluator, takes in Some(AST) or None and
//     returns a recipe
// *)
// let eval expression =
//     match expression with
//     | Some ast -> 
//         match ast with 
//         |Recipe(attribute, Dish(season, dish_type, recipe_exception)) -> 
//             let a = prettyprint (saladGen season recipe_exception) 
//             printf "%s\n" a
//             a

//     | None -> 
//             let b = "Invalid"
//             printf "%s\n" b 
//             b
            