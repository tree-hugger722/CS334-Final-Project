module Parser
open Combinator


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

(*
 function ensures season input is fall/winter/spring/summer, else failure
 *)

let season a = 
    match a with
    | "fall" -> Fall
    | "winter"-> Winter
    | "spring" -> Spring
    | "summer" -> Summer
    | _ -> failwith "Not a season."

(*
 function ensures input is a salad, else failure
 *)

let dish a = 
    match a with
    | "salad" -> Salad
    | _ -> failwith "Not a dish."

let pdish = pstr "salad"

exprImpl := pseq ((pleft (pseason_name) (pws1))) ((pdish)) (fun (a, b) -> Plate(season a, dish b))

(*
 Function parses input and returns success or failure
 *)
let parse(s: string) : Expr option = 
    let input = prepare s
    match grammar input with
    | Success(res, _) -> Some res
    | Failure(_, _) -> None
