open System

let printToConsole message = printfn "%s "message
    
let parseInt (str: string) = 
    match Int32.TryParse str with 
    | (true, v) -> Some v 
    | (false, _) -> None

let parseValidInputArguments consoleInput =
    consoleInput 
    |> Array.toList
    |> List.map (fun v -> parseInt v) 
    |> List.filter(fun v -> v.IsSome)
    |> List.map(fun v -> v.Value)
    |> List.sort

let getGameBounds consoleInput defaultLower defaultUpper =
    let parsedConsoleInput = parseValidInputArguments consoleInput        
    match parsedConsoleInput.Length with 
    | 1 -> (defaultLower, parsedConsoleInput.[0]) 
    | 2 -> (parsedConsoleInput.[0], parsedConsoleInput.[1]) 
    | _ -> (defaultLower, defaultUpper) 

let compare guess answer lowerBound upperBound =
    match guess with 
    | guess when guess < lowerBound -> (false, $"{guess} is below {lowerBound}!")
    | guess when guess > upperBound -> (false, $"{guess} is above {upperBound}!")
    | guess when guess < answer -> (false, $"{guess} is too low!")
    | guess when guess > answer -> (false, $"{guess} is too high!") 
    | guess -> (true, $"{guess} is the correct answer!")

let rec getNumberGuess correctAnswer lowerBound upperBound=
    printf "Your guess: "
    match parseInt (Console.ReadLine()) with 
    | None -> 
        printToConsole "You need to guess integers!"
        getNumberGuess correctAnswer lowerBound upperBound
    | Some guess  -> 
        match compare guess correctAnswer lowerBound upperBound with 
        | (true, victoryMsg) -> victoryMsg
        | (false, errorMsg) -> 
            printToConsole errorMsg
            getNumberGuess correctAnswer lowerBound upperBound

[<EntryPoint>]
let main argv =
    let lowerBound, upperBound = getGameBounds argv 1 50
    let correctAnswer = ((Random()).Next(lowerBound, upperBound)) 
    printToConsole $"I am thinking of a number between {lowerBound} and {upperBound}... guess it!"  
    printToConsole (getNumberGuess correctAnswer lowerBound upperBound)
    0 