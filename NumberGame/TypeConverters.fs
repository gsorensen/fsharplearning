namespace NumberGame 

module TypeConverters =
    open NumberGame.Types 

    let guessFromValue (value: int) = Guess value
    let boundsFromValue (value: int*int) = Bounds value
    let answerFromValue (value: int) = Answer value
    let valueOfGuess (Guess g) = g
    let valueOfAnswer (Answer a) = a 
    let valuesOfBounds (Bounds (l,u)) = (l, u)
    let valuesOfRound (r: Round) =
        match r with 
        | (g, a, b) -> (valueOfGuess g, valueOfAnswer a, valuesOfBounds b)