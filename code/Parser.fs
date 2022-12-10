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
|Spice

type Name = 
|Category of Category
|Name of string
|NoName

type Flag = 
|Include 
|Exclude

type Exception = 
|SoftCore of Flag * List<Name>
|HardCore of Exception * Exception
|NoException

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
    | "salad" -> Salad
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

let singleName a = 
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
    |"spices" -> Category(Spice)
    |str -> Name(str)

let rec name xs = 
    match xs with 
    |[x] -> [singleName x]
    |x::xs' -> 
            let ys = name xs'
            [singleName x] @ ys
    |[] -> [NoName]

let rec convertAttributes xs = 
    match xs with 
    |[x] -> Attribute(temp x)
    |x::xs' -> Attributes(Attribute(temp x), convertAttributes xs')
    |_ -> NoAttribute

//converts list of attributes to readable
(*
let rec convertHardcore xs = 
    match xs with 
    |[x] -> x
    |x::xs' -> HardCore(x, convertHardcore xs')
    |_ -> SoftCore(Include, [Name("confused")])
*)

(*
    Main parsers:
        expr gets implemented later 
        grammar runs expr and parses end of file
 *)

let expr, exprImpl = recparser()
let grammar = pleft expr peof

(*
    Attribute parsers:
        ptemp parses "warm" and "cold" including the space after
        pattribute parses 0 or more instances of ptemp and then returns either
            |Attribute(Temperature)
            |Attributes(Attribute(), Attribute())
 *)
let ptemp = pleft (pstr "warm") (pws1) <|> pleft (pstr "cold") (pws1)
let pattribute = (pmany0 ptemp) |>> convertAttributes

(*
    Season parser:
        parses "fall", "winter", etc. including the space after
        returns Season type
 *)
let pseason = (pleft (pstr "fall" <|> pstr "winter"  <|> pstr "summer" <|> pstr "spring") pws1) |>> season

(*
    DishType parser:
        psalad parses salad, with or without a space after
            (allows for the possibility of not including exceptions)
        pdishtype parses psalad, returns DishType
            only Salad, for now
 *)
let psalad = (pleft (pstr "salad") (pws1)) <|> (pstr "salad")
let pdishtype = psalad |>> dish

(*
    Flag parser:
        parses "with" or "without", including the space after
        returns Include or Exclude (flag type)
 *)
let pflag = (pleft (pstr "without") (pws1) <|> pleft (pstr "with") (pws1)) |>> flag


(*
    Name parsers:
        psinglename parses a sequence of characters, and concatenates into a string
        pmulti parses one or more instances of ", STRING"

        pmanynames combines the above parsers, allowing it to parse a sequence of names
            such as "nuts, cheese, bread" - returns a list of strings
        
        pname parses one or more names, separated by ", " and returns a list of Names
            (Category or strings)
 *)
let psinglename = pmany1 pletter |>> (fun xs -> System.String.Join("", xs))
let pmulti =  pmany1 (pright (pstr ", ") (psinglename))
let pmanynames = pseq (psinglename) (pmulti) (fun (a, b) -> [a] @ b)
let pname = ((pmanynames) <|> ((psinglename) |>> (fun a -> [a]))) |>> name

(*
    Exception parsers:
        psoftcore parses an instance with only one flag and returns SoftCore
            i.e. "with cheese" or "without cheese, nuts, lettuce"
        phardcore parses an instance with two flags and returns HardCore(SoftCore, SoftCore)
            i.e. "with cheese, nuts and without lettuce"

        pexception combines the two and parses either a single exception or two expections
 *)
 //LEAVING OFF: PARSER WORKS, BUT DOES NOT INCLUDE CASE WHERE NO EXCEPTIONS ARE ENTERED
let psoftcore = pseq (pflag) (pname) (fun (a, b) -> SoftCore(a, b))
let phardcore = pseq (pleft (psoftcore) (pstr " and ")) (psoftcore) (fun (a, b) -> HardCore(a, b))
let pexception = phardcore <|> psoftcore 

(*
    Dish parser:
        pdish
 *)
let pdish = pseq (pseq (pseason) (pdishtype) (fun (a, b) -> (a, b))) (pexception) (fun ((a, b), c) -> Dish(a, b, c))

(*
    Implememt expr by combining attribute and dish parsers
        Outputs Type Recipe
 *)
exprImpl:= pseq (pattribute) (pdish) (fun (a, b) -> Recipe(a, b))


(*
    Function parses input and returns success or failure
 *)

let parse(s: string) : Expr option = 
    let input = prepare s
    match grammar input with
    | Success(res, _) -> Some res
    | Failure(_, _) -> None
