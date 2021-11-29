type GuessResult = | BelowLowerBound | BelowAnswer | Correct | AboveUpperBound | AboveAnswer 

let printToConsole message = printfn "%s "message
    
let parseInt (str: string) = 
    match System.Int32.TryParse str with 
    | (true, v) -> Some v 
    | (false, _) -> None

let parseValidInputArguments (input: string[]) =
    input 
    |> Array.toList 
    |> List.map (fun v -> parseInt v) |> List.filter(fun v -> v.IsSome) |> List.map(fun v -> v.Value) 
    |> List.sort

let getGameBounds consoleInput defaultLower defaultUpper =
    let parsedConsoleInput = parseValidInputArguments consoleInput        
    match parsedConsoleInput.Length with 
    | 1 when parsedConsoleInput.[0] > defaultLower -> (defaultLower, parsedConsoleInput.[0]) 
    | 2 -> (parsedConsoleInput.[0], parsedConsoleInput.[1]) 
    | _ -> (defaultLower, defaultUpper) 

let checkGuess (guess: int) (answer: int) (bounds: int*int) : GuessResult =
    match (guess, answer) with 
    | (guess, answer) when guess > answer ->
        match (guess, snd bounds) with 
        | (guess, upperBound) when guess > upperBound -> AboveUpperBound
        | _ -> AboveAnswer
    | (guess, answer) when guess < answer ->
        match (guess, fst bounds) with 
        | (guess, lowerBound) when guess < lowerBound -> BelowLowerBound 
        | _ -> BelowAnswer
    | _ -> Correct

let resultString (result: GuessResult) = 
    match result with 
    | Correct -> "That is the correct answer!"
    | AboveAnswer -> "That number is too high!"
    | AboveUpperBound -> "That number is above the upper bound!"
    | BelowAnswer -> "That number is too low!"
    | BelowLowerBound -> "That number is below the lower bound!"

let displayResult (result: GuessResult) = resultString result |> printToConsole

let rec getNumberGuess (correctAnswer: int) (bounds: int*int) =
    match parseInt (System.Console.ReadLine()) with 
    | None -> 
        printToConsole "You need to guess integers!"
        getNumberGuess correctAnswer bounds
    | Some guess  -> 
        let guessResult = checkGuess guess correctAnswer bounds
        displayResult guessResult
        match guessResult with 
        | Correct -> 0
        | _ -> getNumberGuess correctAnswer bounds

[<EntryPoint>]
let main argv =
    let bounds = getGameBounds argv 1 50
    let correctAnswer = ((System.Random()).Next(fst bounds, snd bounds)) 
    printToConsole $"I am thinking of a number between {fst bounds} and {snd bounds}... guess it!"  
    getNumberGuess correctAnswer bounds