module Parser
open Combinator

type Season = 
|Fall | Winter | Spring | Summer

type Green = 
|Kale of "Kale" * Fall
|Arugula of "Arugula" * Spring
|Spinach of "Spinach" * Winter




type Salad = green * vegetable * dressing

type Dish = 
|salad of Salad

type Expr = 
|season of Season
|