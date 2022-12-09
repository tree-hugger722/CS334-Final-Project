module Parser2
open Combinator 


type IngredientName = 
|ingredientName of String


type Category = 
|green
|cheese
|nut
|dressing
|fruit
|vegetable
|grain 
|onion
|herb
|legume

type Name = 
|category of Category
|name of IngredientName
|combo of Name * Name

type Flag = 
|Include 
|Exclude

type Exception = 
|softCore of Flag * Name
|hardCore of Exception * Exception

type DishType = 
|salad

type Season =
| fall
| winter
| spring 
| summer 

type Dish = 
|dish of Season * DishType * Exception 

type Temperature = 
|Warm 
|Cold

type Attribute = 
|temp of Temperature 

type expr = 
|Recipe of Attribute * Dish 


let expr, exprImpl = recparser()

let grammar = pleft expr peof

let pseason_name = pstr "fall" <|> pstr "winter"  <|> pstr "summer" <|> pstr "spring" 

let pdish = pstr "salad" 

let ptemp = pstr "warm" <|> pstr "cold"

let pinclusion = pstr "with" <|> pstr "without"

let pingredient = 

exprImpl := pseq ((pleft (pseason_name) (pws1))) ((pdish)) (fun (a, b) -> Plate(season a, dish b))