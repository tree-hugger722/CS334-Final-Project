module DataReader
open Parser
open FSharp.Data
open AST


(*
    Code to process the CSV file containing the list of possible ingredients for recipes
    Opens the CSV file and creates a list of objects of type Ingredient
    Inspired by Dan Barowy's Movie Type code (with his permission)
*)

// Unit type describes the units in which an Ingredient is measured 
type Unit = 
|Cup
|Tablespoon
|Ounce
|Liter
|Head
|Whole
|Ear
|Bunch
|Clove


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
    Ingredient type: take a string for ingredient name, a decimal for quantity (serving size for one person), a
    unit for measurement, list of seasons during which it can be used, and food category
 *)
type Ingredient = {Name:string; Quantity:float; Unit:Unit; Season_List: Season List; Category: Category}

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
    |"Spice" -> Spice
    |"Herb" -> Herb
    |"Legume" -> Legume
    |"Onion" -> Onion
    |"Grain" -> Grain
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
    |"Clove" -> Clove
    | _ -> failwith "Undefined unit of food"

// Organize data from CSV into a list of Ingredients
let formatInput = 
    ingredients.Rows 
        |> Seq.map (fun row -> {Name=row.Name; Quantity= float(row.Quantity); Unit=(convertToUnit row.Unit); Season_List=(convertBoolToSeason ([]) ([row.Fall; row.Winter; row.Summer; row.Spring])); Category=(convertToCategory row.Category);})
        |> Seq.sortBy (fun ingredient -> ingredient.Category)
        |> Seq.toList

// Test function to print out ingredient type
let printIngredients (rows: Ingredient list) =
    for ingredient in rows do
        printfn "%A" ingredient
       //printfn "%s, %f, %A, %A, %A" ingredient.Name ingredient.Quantity ingredient.Unit ingredient.Season_List ingredient.Category