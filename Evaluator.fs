module Evaluator
open Parser
open FSharp.Data

(*
To-Dos:
- type Ingredient = Quantity * Unit * Season List * category
- Populate the CSV with all the ingredients and their seasons and ingredient category (ie cheese, nut, green, fruit)
- Build out evaluator that creates an Ingredient from each CSV row and define the following functions:
    - pick a random ingredient from a global list of Ingredients of a certain ingredient category (parameters: ingredient category)
            - return ingredient
    - salad generator --> calls generator function, get ingredients, and return output of "Here is your salad: "
        - assuming salad generator for one person
Anna: writing the salad generator
Emma: populate the CSV manually, write code to iterate through the CSV and create ingredient types, write the code to pick an ingredient

*)

type Category = 
|Green
|Cheese
|Nut
|Dressing
|Fruit
|Vegetable

type Unit = 
|Cup
|Tablespoon
|Ounce
|Liter
|Head
|Whole
|Ear
|Bunch

(*
    Code to process the CSV file containing the list of possible ingredients for recipes
    Opens the CSV file and creates a list of objects of type Ingredient
    Inspired by Dan Barowy's Movie Type code (with his permission)
*)
// Starts looking for CSVs in the same directory as my source code
[<Literal>]
let ResolutionFolder = __SOURCE_DIRECTORY__

[<Literal>]
let IngredientCSV = "Ingredients.csv"

// Dynamically-created Ingredient type
type DynamicIngredient = CsvProvider<IngredientCSV, ResolutionFolder=ResolutionFolder>

// Load data using the Ingredient type provider
let ingredients = DynamicIngredient.Load(ResolutionFolder + "/" + IngredientCSV)

(*
    ingredient type: take a string for ingredient name, an int for quantity (serving size for one person), a
    unit for measurement, list of seasons during which it can be used, and food category
 *)
type Ingredient = {Name:string; Quantity:decimal; Unit:Unit; Season_List: Season List; Category: Category}

// Converts list of four booleans to seasons, according to the CSV season input
let rec convertBoolToSeason (season_list: Season list) (bool_list: bool list)=
    match bool_list with
    | [true; _; _; _] -> convertBoolToSeason (Fall::season_list) (bool_list[1..])
    | [false; _; _; _] -> convertBoolToSeason (season_list) (bool_list[1..])
    | [true; _; _] -> convertBoolToSeason (Winter::season_list) (bool_list[1..])
    | [false; _; _] -> convertBoolToSeason (season_list) (bool_list[1..])
    | [true; _] -> convertBoolToSeason (Summer::season_list) (bool_list[1..])
    | [false; _] -> convertBoolToSeason (season_list) (bool_list[1..])
    | [true] -> Spring::season_list
    | [false] -> season_list
    | _ -> failwith "Incorrectly typed season list entered"

// Converts string to Category Type
let convertToCategory (input: string) =
    match input with
    |"Greens" -> Green
    |"Cheese" -> Cheese
    |"Nut" -> Nut
    |"Dressing" -> Dressing
    |"Fruit" -> Fruit
    |"Vegetable" -> Vegetable
    | _ -> failwith "Undefined food category"

// Converts string to Unit Type
let convertToUnit (input: string) =
    match input with
    |"Cup" -> Cup
    |"Tablespoon" -> Tablespoon
    |"Ounce" -> Ounce
    |"Liter" -> Liter
    |"Head" -> Head
    |"Whole" -> Whole
    |"Ear" -> Ear
    |"Bunch" -> Bunch
    | _ -> failwith "Undefined unit of food"

// Organize data from CSV
let convertRows = 
        ingredients.Rows 
            |> Seq.map (fun row -> {Name=row.Name; Quantity=row.Quantity; Unit=(convertToUnit row.Unit); Season_List=(convertBoolToSeason ([]) ([row.Fall; row.Winter; row.Summer; row.Spring])); Category=(convertToCategory row.Category);})
            |> Seq.sortBy (fun ingredient -> ingredient.Category)
            |> Seq.toList

// Test function to print out ingredient type
let printIngredients (rows: Ingredient list) =
    for ingredient in rows do
        printfn "%A" ingredient
       //printfn "%s, %f, %A, %A, %A" ingredient.Name ingredient.Quantity ingredient.Unit ingredient.Season_List ingredient.Category

(*
let getFallIngredients (rows: list<string * int * string * bool * bool * bool * bool * string>) =
    List.map(fun (name,quantity,unit,isFall,isWinter,isSummer,isSpring,category) ->  Ingredient(name,quantity,unit,(convertBoolToSeason ([])([isFall; isWinter; isSummer; isSpring])),category)) rows
*)
(*
    ingredient generator: takes in a food category and returns a random ingredient from that category in the CSV
 *)
(*
let ingredientGen (foodCat: Category) : Ingredient = 
    let i: Ingredient = Ingredient("lettuce", 0, Cup, [Fall], Green)
    i

(*
    salad generator: returns a list of ingredients
 *)
let saladGen : Ingredient list = 
    let i: Ingredient = Ingredient("lettuce", 0, Cup, [Fall], Green)
    [i; i; i; i]

(*
    helper method that prints out a single ingredient
 *)
let ingredientPrint (i: Ingredient) = 
    match i with
    |Ingredient(n, q, u, s, c) -> 
        if q > 1 || q = 0 then
            printf "%A %As of %s\n" q u n
        else
            printf "%A %A of %s\n" q u n

(*
    prints out a list of ingredients
    NOTE: I get a warning when i call this function because it thinks the expression is missing arguments?
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
*)



