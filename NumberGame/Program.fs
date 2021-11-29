type GuessResult = | BelowLowerBound | BelowAnswer | Correct | AboveUpperBound | AboveAnswer 

type GuessInput = int * int * int * int 

/// TODO: Add single case union types for self-documenting code

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

let getGameBounds (consoleInput: string[]) (defaultLower: int) (defaultUpper: int) =
    let parsedConsoleInput = parseValidInputArguments consoleInput        
    match parsedConsoleInput.Length with 
    | 1 when parsedConsoleInput.[0] > defaultLower -> (defaultLower, parsedConsoleInput.[0]) 
    | 2 -> (parsedConsoleInput.[0], parsedConsoleInput.[1]) 
    | _ -> (defaultLower, defaultUpper) 

let checkGuess (input: GuessInput): GuessResult = 
    match input with 
    | (guess, _, lowerBound, _) when guess < lowerBound -> BelowLowerBound
    | (guess, answer, _, _) when guess < answer -> BelowAnswer
    | (guess, _, _, upperBound) when guess > upperBound -> AboveUpperBound
    | (guess, answer, _, _) when guess > answer -> AboveAnswer
    | _ -> Correct

let resultString (result: GuessResult) = 
    match result with 
    | Correct -> "That is the correct answer!"
    | AboveAnswer -> "That number is too high!"
    | AboveUpperBound -> "That number is above the upper bound!"
    | BelowAnswer -> "That number is too low!"
    | BelowLowerBound -> "That number is below the lower bound!"

let displayResult (result: GuessResult) = resultString result |> printToConsole

let rec guessNumber (correctAnswer: int) (bounds: int*int) (statusMessage: string) =
    printf "%s\nYour guess: " statusMessage
    match parseInt (System.Console.ReadLine()) with 
    | None -> 
        guessNumber correctAnswer bounds "You need to guess integers!"
    | Some guess  -> 
        match checkGuess (guess, correctAnswer, fst bounds, snd bounds) with 
        | Correct -> resultString Correct
        | other -> guessNumber correctAnswer bounds (resultString other)

[<EntryPoint>]
let main argv =
    let bounds = getGameBounds argv 1 50
    let correctAnswer = ((System.Random()).Next(fst bounds, snd bounds)) 
    guessNumber correctAnswer bounds $"I am thinking of a number between {fst bounds} and {snd bounds}... guess it!" 
    |> printToConsole
    0