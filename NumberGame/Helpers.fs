namespace NumberGame 

module Helpers = 
    let printToConsole (message: string) = printfn "%s "message
        
    let parseInt (str: string) = 
        match System.Int32.TryParse str with 
        | (true, v) -> Some v 
        | (false, _) -> None

    let parseValidInputArguments (input: string[]) =
        input 
        |> Array.toList 
        |> List.map (fun v -> parseInt v) |> List.filter(fun v -> v.IsSome) |> List.map(fun v -> v.Value) 
        |> List.sort
