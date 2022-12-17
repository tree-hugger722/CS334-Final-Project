module Parser
open Combinator 
open System
open AST


(*
    Main parsers:
        expr gets implemented later 
        grammar runs expr and parses end of file
 *)

let expr, exprImpl = recparser()
let grammar = pleft expr peof

(*
    Attribute parsers:
        temp takes string and matches with Warm or Cold or fails

        convertAttributes takes list of strings and turns into Attribute or
            Attributes.  Will return Attribute(NoAttribute) if no attribute is parsed

        ptemp parses "warm" and "cold" including the space after

        pattribute parses 0 or more instances of ptemp and then returns either
            |Attribute(Temperature)
            |Attributes(Attribute(), Attribute())
 *)
let temp a = 
    match a with 
    |"warm" -> Warm
    |"cold" -> Cold
    |_ -> failwith "Not a valid temperature"

let rec convertAttributes xs = 
    match xs with 
    |[x] -> AttributeOne(temp x)
    |x::xs' -> Attributes(AttributeOne(temp x), convertAttributes xs')
    |_ -> NoAttribute

let ptemp = pleft (pstr "warm") (pws1) <|> pleft (pstr "cold") (pws1)
let pattribute = (pmany0 ptemp) |>> convertAttributes

(*
    Season parser: 
        season takes in a string and returns Season type

        pseason parses "fall", "winter", etc. including the space after
        returns Season type
 *)
let season a = 
    match a with
    | "fall" -> Fall
    | "winter"-> Winter
    | "spring" -> Spring
    | "summer" -> Summer
    | _ -> failwith "Not a season."

let pseason = (pleft (pstr "fall" <|> pstr "winter"  <|> pstr "summer" <|> pstr "spring") pws1) |>> season

(*
    DishType parser: 
        dish takes string input and returns dishType (salad) or fails

        psalad parses salad, with or without a space after
            (allows for the possibility of not including exceptions)
        pdishtype parses psalad, returns DishType
            only Salad, for now
 *)

let dish a = 
    match a with
    | "salad" -> Salad
    | _ -> failwith "Not a dish."


let psalad = (pleft (pstr "salad") (pws1)) <|> (pstr "salad")
let pdishtype = psalad |>> dish

(*
    Flag parser: 
        flag takes a string input and returns Flag type
            Include or Exclude
        
        pflag parses "with" or "without", including the space after
            returns Include or Exclude 
 *)
let flag a = 
    match a with 
    |"with" -> Include
    |"without" -> Exclude
    |_ -> failwith "please use the words 'with' or 'without'"

let pflag = (pleft (pstr "without") (pws1) <|> pleft (pstr "with") (pws1)) |>> flag


(*
    Name helper functions: 
        singleName takes a string and returns Name type
            either Name(Category()) or Name(string())
        
        name takes list of strings and returns list of Names

        combineManyStrings takes a string and a list of strings
            and combines into a single string separated by " "
        
        combineCharListList combines a char list list and returns
            a single string
 *)

let singleName (a: string) = 
    let b = a.ToLower()
    match b with
    |"greens" -> Cat(Green)
    |"cheese" -> Cat(Cheese)
    |"nuts" -> Cat(Nut)
    |"dressing" -> Cat(Dressing)
    |"fruit" -> Cat(Fruit)
    |"vegetable" -> Cat(Vegetable)
    |"grains" -> Cat(Grain) 
    |"onion" -> Cat(Onion)
    |"herbs" -> Cat(Herb)
    |"legumes" -> Cat(Legume)
    |"spices" -> Cat(Spice)
    |str -> StrName(str)

let rec name xs = 
    match xs with 
    |[x] -> [singleName x]
    |x::xs' -> 
            let ys = name xs'
            [singleName x] @ ys
    |[] -> [NoName]

let rec combineManyStrings (str: string, xs: list<string>): string = 
    match xs with
    |[x] -> x
    |x :: xs' ->
                let ys = combineManyStrings (str, xs')
                str + " " + x + " " + ys
    |_ -> failwith "Something going wrong with names of multiple words"

let rec combineCharListList (cs: char list list): string = 
    match cs with
    |[] -> ""
    |[cl] -> stringify cl
    |cl::cls -> stringify cl + combineCharListList cls

(*
    Name parsers:
        psinglename parses a sequence of characters, and concatenates into a string
        
        pquote parses letters/whitespace in between two quotes, and returns a single string
            i.e. "\"butternut squash\""

        pstring parses either a singleName or a quote

        pmulti parses one or more instances of ", STRING"

        pmanynames combines the above parsers, allowing it to parse a sequence of names
            such as "nuts, cheese, bread" - returns a list of strings
        
        pname parses one or more names, separated by ", " and returns a list of Names
            (Category or strings)
 *)

let psinglename = pmany1 pletter |>> (fun xs -> System.String.Join("", xs))
let pquote = pright (pstr"(") (pleft (pmany1 ((pmany1 pletter) <|> pws1)) (pstr")")) |>> combineCharListList
let pstring = (pquote <|> psinglename)
let pmulti =  pmany1 (pright (pstr ", ") (pstring))
let pmanynames = pseq (pstring) (pmulti) (fun (a, b) -> [a] @ b)
let pname = ((pmanynames) <|> ((pstring) |>> (fun a -> [a]))) |>> name


(*
    Exception parsers: all return Exception type
        psoftcore parses an instance with only one flag and returns SoftCore
            i.e. "with cheese" or "without cheese, nuts, lettuce"
        
        phardcore parses an instance with two flags and returns HardCore(SoftCore, SoftCore)
            i.e. "with cheese, nuts and without lettuce"
       
        pnoexception parses instance where no exceptions are entered and returns NoException

        pexception combines the two and parses either a single exception or two expections
 *)

let psoftcore = pseq (pflag) (pname) (fun (a, b) -> SoftCore(a, b))
let phardcore = pseq (pleft (psoftcore) (pstr " and ")) (psoftcore) (fun (a, b) -> HardCore(a, b))
let pnoexception = (peof) |>> (fun a -> NoException)
let pexception = phardcore <|> psoftcore <|> pnoexception

(*
    Dish parser:
        pdish combines season, dishType, and exception parsers and returns Dish type
 *)
let pdish = pseq (pseq (pseason) (pdishtype) (fun (a, b) -> (a, b))) (pexception) (fun ((a, b), c) -> Dish(a, b, c))

(*
    Implememt expr by combining attribute and dish parsers
        Outputs Recipe type
 *)
exprImpl:= pseq (pattribute) (pdish) (fun (a, b) -> Recipe(a, b))


(*
    Function parses input and returns success or failure
 *)

let parse(s: string) : Expr option = 
    let input = prepare s
    match grammar input with
    | Success(res, _) -> Some res
    | Failure(_, _) -> failwith "usage: please input in correct formula. Some examples are: 
    cold spring salad
    warm winter salad with nuts, (artichokes)"
