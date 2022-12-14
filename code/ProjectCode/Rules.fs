module Rules
(*
    Rules printer
 *)

let printRules = 
    printfn "Before we get started, we would like to lay down a few ground rules so that you can 
get the salad of your dreams. Our program takes a specifc format so that it doesn't get confused
and accidentally output something else, like a steak recipe or focaccia (ewwww, meat and gluten). \n"

    printfn "However, we know you're hungry, so we will keep this brief. \n"

    printfn "Our salad has three categories you can personalize: attributes, season, and inclusions/exclusions. 
Please note that you don't have to inclue attributes or inclusions/exclusions, but you must include a season. \n"

    printfn "Our current attributes are temperatures: either warm or cold. \n"

    printfn "Season options are fall, winter, spring, and summer. \n"

    printfn "If you want to include a specific ingredient, you must write 'with (INGREDIENT_NAME)"

    printfn "If you want to exclude a specific ingredient, or better yet, an entire category of ingredients, you 
    must write 'without nuts"

    printfn "You can include and exclude things at the same time, or even include/exclude multiple things separated
    by commas.  For example 'with (Kale) and without cheese' or 'with (Romaine), (Butternut Squash) and without cheese, nuts'
    are two viable inclusions/exclusions. \n"

    printfn "NOW, what you've been waiting for - writing a program!  Please use the format
    <attribute> <season> salad <inclusions/exclusion>. Here are some example programs: 
    
    (1) fall salad
    (2) spring salad with (Pecorino Romano)
    (3) warm winter salad without vegetables 
    (4) cold summer salad with (arugula) and without nuts "

    printfn "Let's get cooking!"