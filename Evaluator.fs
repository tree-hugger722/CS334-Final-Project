module Evaluator
open Parser

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

(*
    ingredient type: take a string for ingredient name, an int for quantity (serving size for one person), a
    unit for measurement, list of seasons during which it can be used, and food category
 *)
type Ingredient = Ingredient of string * int * Unit * Season List * Category

(*
    ingredient generator: takes in a food category and returns a random ingredient from that category in the CSV
 *)
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




