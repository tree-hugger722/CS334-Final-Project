module AST

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
|Cat of Category
|StrName of string
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
|AttributeOne of Temperature 
|Attributes of Attribute * Attribute
|NoAttribute 

type Expr = 
|Recipe of Attribute * Dish 