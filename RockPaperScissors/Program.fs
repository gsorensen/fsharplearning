open System

let intFromString (str : string) =
    match System.Int32.TryParse(str) with 
    | (true, value) -> value 
    | (false, _) -> 3  

let output str = 
    printfn "%s" str 

let randomChoice (random: Random) : int =
    random.Next(0, 3)

type Move =
    | Rock 
    | Paper 
    | Scissors
    | Invalid

type Result =
    | Draw 
    | PlayerWin
    | ComputerWin

type Game = {
    playerScore: int
    computerScore: int 
    bestTo: int
}

let init bestTo = {
    playerScore = 0;
    computerScore = 0;
    bestTo = bestTo
}

let moveFromInt intVal = 
    match intVal with
    | 0 -> Rock 
    | 1 -> Paper 
    | 2 -> Scissors
    | _ -> Invalid 
    
let evaluateRound playerChoice computerChoice : string * Result =
    match (playerChoice, computerChoice) with 
    | (Invalid, _) | (_,  Invalid) -> ("There are only three options!", Draw)
    | (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) -> ("It's a draw!", Draw)
    | (Rock, Paper) -> ("Paper beats rock. Computer wins!", ComputerWin)
    | (Paper, Scissors) -> ("Scissors beat paper. Computer wins!", ComputerWin)
    | (Scissors, Rock) -> ("Rock beats scissors. Computer wins!", ComputerWin)
    | (Paper, Rock) -> ("Paper beats rock. Player wins!", PlayerWin)
    | (Scissors, Paper) -> ("Scissors beat paper. Player wins!", PlayerWin)
    | (Rock, Scissors) -> ("Rock beats scissors. Player wins!", PlayerWin)

let userInput ()  = 
    printfn "Rock (0), paper (1) or scissors (2) ?"
    System.Console.ReadLine() 
    |> intFromString
    |> moveFromInt

let rec round rng game =  
    printfn "Best to %d. Player: %d Computer %d" game.bestTo game.playerScore game.computerScore
    match (game.playerScore, game.computerScore) with 
    | (3, _) -> "Game over, player wins!"
    | (_, 3) -> "Game over, computer wins!"
    | (_, _) ->
        let computerChoice = randomChoice rng |> moveFromInt 
        let playerChoice = userInput()
        match evaluateRound playerChoice computerChoice with 
        | (resultString, v) ->
            printfn "%s" resultString
            match v with 
            | PlayerWin ->
                {game with Game.playerScore = game.playerScore + 1} |> round rng 
            | ComputerWin ->
                {game with Game.computerScore = game.computerScore + 1} |> round rng 
            | Draw -> 
                game |> round rng 

[<EntryPoint>]
let main _ = 
    let rng = Random()
    init 3 
    |> round rng 
    |> printfn "%s"
    0