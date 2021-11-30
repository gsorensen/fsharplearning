namespace NumberGame 

module TypeConverters =
    open NumberGame.Types 

    let guessFor (value: int) = Guess value
    let boundsFor (value: int*int) = Bounds value
    let answerFor (value: int) = Answer value
    let valueOfGuess (Guess g) = g
    let valueOfAnswer (Answer a) = a 
    let valuesOfBounds (Bounds (l,u)) = (l, u)
    let valuesOfRound (r: Round) =
        match r with 
        | (g, a, b) -> (valueOfGuess g, valueOfAnswer a, valuesOfBounds b)