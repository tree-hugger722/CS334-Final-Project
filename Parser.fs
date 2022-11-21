module Parser
open Combinator

type Season = 
|Fall | Winter | Spring | Summer

type Dish = 
|Salad

type Expr = 
|Plate of Season * Dish

//let expr, exprImpl = recparser()

//let grammar = pleft expr peof

//let par = pmany1 pletter |>> stringify



//let pplate = pseq (pseq ((pmany1 pletter |>> stringify) (pws1))) (pmany1 pletter |>> stringify) (fun a b -> plate(a, b))

