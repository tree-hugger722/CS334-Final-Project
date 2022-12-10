module Parser
open Combinator 

type Category = 
|Green
|Cheese
|Nut
|Dressing
|Fruit
|Vegetable
|Grain 
|Onion
|Herb
|Legume

type Name = 
|Category of Category
|Name of string
|Combo of Name * Name
|NoName

type Flag = 
|Include 
|Exclude

type Exception = 
|SoftCore of Flag * List<Name>
|HardCore of Exception * Exception

type DishType = 
|Salad

type Season =
| Fall
| Winter
| Spring 
| Summer 

type Dish = 
|Dish of Season * DishType * Exception 

type Temperature = 
|Warm 
|Cold

type Attribute = 
|Attribute of Temperature 
|Attributes of Attribute * Attribute
|NoAttribute 

type Expr = 
|Recipe of Attribute * Dish 

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
    | "salad " -> Salad
    | _ -> failwith "Not a dish."

let temp a = 
    match a with 
    |"warm" -> Warm
    |"cold" -> Cold
    |_ -> failwith "Not a valid temperature"

let flag a = 
    match a with 
    |"with" -> Include
    |"without" -> Exclude
    |_ -> failwith "please use the words 'with' or 'without'"

let name a = 
    match a with
    |"greens" -> Category(Green)
    |"cheese" -> Category(Cheese)
    |"nuts" -> Category(Nut)
    |"dressing" -> Category(Dressing)
    |"fruit" -> Category(Fruit)
    |"vegetable" -> Category(Vegetable)
    |"grains" -> Category(Grain) 
    |"onion" -> Category(Onion)
    |"herbs" -> Category(Herb)
    |"legumes" -> Category(Legume)
    |str -> Name(str)

//converts a list of softcore exceptions into hardcore exception
let rec convertAttributes xs = 
    match xs with 
    |[x] -> Attribute(temp x)
    |x::xs' -> Attributes(Attribute(temp x), convertAttributes xs')
    |_ -> NoAttribute

//converts list of attributes to readable
let rec convertHardcore xs = 
    match xs with 
    |[x] -> x
    |x::xs' -> HardCore(x, convertHardcore xs')
    |_ -> SoftCore(Include, [Name("confused")])


//let expr, exprImpl = recparser()

//let grammar = pleft expr peof

let ptemp = pstr "warm " <|> pstr "cold "

let pattribute = (pmany0 ptemp)   

let pseas = pstr "fall" <|> pstr "winter"  <|> pstr "summer" <|> pstr "spring" 

let pseason = pleft pseas pws1

let pdishType = pstr "salad " 

let pflag = pstr "without" <|> pstr "with"

let pname = pmany1 pletter |>> (fun xs -> System.String.Join("", xs))

//parse flag, ignore space, parse name and return softfore(flag, name)
//this doesn't include with cheese, nuts or with a and without b
let psoftcore = pseq (pleft (pflag) (pws1)) (pname) (fun (a,b) -> SoftCore(flag a, [name b]))


//let pdish = pseq (pseq (pseason) (pdishType) (fun (a, b) -> (a, b))) (psoftcore) (fun ((a, b), c) -> Dish(season a, dish b, c))

//exprImpl:= pseq (pattribute) (pdish) (fun (a,b) -> Recipe(temp a, b))

//exprImpl:= Recipe(Attribute(Warm), pdish)

(*
 Function parses input and returns success or failure
 *)

 (*
let parse(s: string) : Expr option = 
    let input = prepare s
    match grammar input with
    | Success(res, _) -> Some res
    | Failure(_, _) -> None
*)
