open Combinator
open Parser
open Evaluator

[<EntryPoint>]

let main args = 
    if args.Length < 1 then
        printfn "Usage: dotnet run <season>"
        printfn "Season options: fall, winter, spring, summer"
        exit 1
    let expression = parse args.[0]
    match expression with
    | Some ast -> printf "%A\n" (prettyprint (eval ast))
    | None -> printfn "Invalid program"
    0