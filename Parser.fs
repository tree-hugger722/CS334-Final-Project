module Parser
open Combinator

(*
To-Dos:
- Same type initialization for Dish as for Season
- type Ingredient = Quantity * Unit * Season List * category
- Populate the CSV with all the ingredients and their seasons and ingredient category (ie cheese, nut, green, fruit)
- Build out evaluator that creates an Ingredient from each CSV row and define the following functions:
    - pick a random ingredient from a global list of Ingredients of a certain ingredient category (parameters: ingredient category)
            - return ingredient
    - salad generator --> calls generator function, get ingredients, and return output of "Here is your salad: "
        - assuming salad generator for one person
Anna: finishing parser, and writing the salad generator
Emma: populate the CSV manually, write code to iterate through the CSV and create ingredient types, write the code to pick an ingredient

*)

type Season =
| Fall
| Winter
| Spring 
| Summer 

type Dish = 
| Salad

type Expr = 
|Plate of Season * Dish

let expr, exprImpl = recparser()

let grammar = pleft expr peof

let pseason_name = pstr "fall" <|> pstr "winter" <|> pstr "summer" <|> pstr "spring"

let season a = 
    match a with
    | "fall" -> Fall
    | "winter"-> Winter
    | "spring" -> Spring
    | "summer" -> Summer
    | _ -> failwith "Not a season."

let dish a = 
    match a with
    | "salad" -> Salad
    | _ -> failwith "Not a dish."

let pdish = pstr "salad"

exprImpl := pseq ((pleft (pseason_name) (pws1))) ((pdish)) (fun (a, b) -> Plate(season a, dish b))

let parse(s: string) : Expr option = 
//turns string into input using prepare
    let input = prepare s
//if parse is successful, return expr option, else return none
    match grammar input with
    | Success(res, _) -> Some res
    | Failure(_, _) -> None
