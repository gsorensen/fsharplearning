namespace NumberGame

module Game =
    open NumberGame.TypeConverters
    open NumberGame.Helpers
    open NumberGame.Logic

    [<EntryPoint>]
    let main argv =
        let bounds = boundsFrom argv 
        let correctAnswer = answerBetween bounds
        guessNumber correctAnswer bounds $"I am thinking of a number between {valuesOfBounds bounds}... guess it!" 
        |> printToConsole
        0