namespace NumberGame 

module Logic = 
    open NumberGame.Types
    open NumberGame.TypeConverters
    open NumberGame.Helpers

    let boundsFrom (consoleInput: string[]) : Bounds =
        let (defaultLower, defaultUpper) = (1, 50)
        let parsedConsoleInput = parseValidInputArguments consoleInput        
        match parsedConsoleInput.Length with 
        | 1 when parsedConsoleInput.[0] > defaultLower -> (defaultLower, parsedConsoleInput.[0]) 
        | 2 -> (parsedConsoleInput.[0], parsedConsoleInput.[1]) 
        | _ -> (defaultLower, defaultUpper) 
        |> boundsFromValue

    let answerBetween (bounds: Bounds) : Answer = 
        match valuesOfBounds bounds with 
        | (lower, upper) -> 
            answerFromValue ((System.Random()).Next(lower, upper))

    let checkGuess (r: Round) : RoundResult = 
        match valuesOfRound r with 
        | (guess, _, (lowerBound, _)) when guess < lowerBound -> BelowLowerBound
        | (guess, answer, _) when guess < answer -> BelowAnswer
        | (guess, _, (_, upperBound)) when guess > upperBound -> AboveUpperBound
        | (guess, answer, _) when guess > answer -> AboveAnswer
        | _ -> Correct

    let resultStringFor (result: RoundResult) : string = 
        match result with 
        | Correct -> "That is the correct answer!"
        | AboveAnswer -> "That number is too high!"
        | AboveUpperBound -> "That number is above the upper bound!"
        | BelowAnswer -> "That number is too low!"
        | BelowLowerBound -> "That number is below the lower bound!"

    let displayResult (result: RoundResult) = resultStringFor result |> printToConsole
    
    let rec guessNumber (correctAnswer: Answer) (bounds: Bounds) (statusMessage: string) : string =
        printf "%s\nYour guess: " statusMessage
        match parseInt (System.Console.ReadLine()) with 
        | None -> 
            guessNumber correctAnswer bounds "You need to guess integers!"
        | Some value  -> 
            match checkGuess (guessFromValue value, correctAnswer, bounds) with 
            | Correct -> resultStringFor Correct
            | other -> guessNumber correctAnswer bounds (resultStringFor other)
