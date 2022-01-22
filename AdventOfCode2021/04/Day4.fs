module Day4 

open System
open System.Text.RegularExpressions

type BingoNumber = BingoNumber of int*bool
type BingoBoard = BingoBoard of BingoNumber list list

type Game = {
    NumbersDrawn: int list;
    BingoBoards: BingoBoard list
}

let readPuzzleInputFromFile filePath =
    System.IO.File.ReadAllLines(filePath) 
    |> Array.toList

let intFrom (s: string) = 
    match Int32.TryParse s with 
    | (true, v) -> Some v 
    | (false, _) -> None

let parseNumbersDrawn (puzzleData: string list) : int list * string list =
    let numbersDrawn = 
        Regex.Split(puzzleData.Head, ",")
        |> Array.toList
        |> List.map(fun s -> intFrom s)
        |> List.filter(fun v -> v.IsSome)
        |> List.map (fun v -> v.Value)
    let bingoBoardInput = puzzleData.Tail
    (numbersDrawn, bingoBoardInput)

let bingoNumbersFromString (s: string) =
    Regex.Split(s, " ") 
    |> Array.map (fun s -> intFrom s)
    |> Array.filter (fun v -> v.IsSome)
    |> Array.map (fun v -> v.Value)
    |> Array.map (fun v -> BingoNumber(v, false))
    |> Array.toList

let bingoBoardFromStringList (rawInput: string list) =
    rawInput 
    |> List.map (fun s -> bingoNumbersFromString s)
    |> BingoBoard    
    
let rec bingoBoardsFromStringList (bingoBoards: BingoBoard list) (idx: int) (rawData: string list) : BingoBoard list =
    let newBingoBoard = rawData[idx+1..idx+5] |> bingoBoardFromStringList 
    match idx+6 > rawData.Length with 
    | true -> (newBingoBoard::bingoBoards).Tail
    | false -> bingoBoardsFromStringList (newBingoBoard::bingoBoards) (idx+6) rawData

let parseBingoBoards (numbersDrawnAndRawBoards: int list * string list)  : int list * BingoBoard list =
    (fst numbersDrawnAndRawBoards, bingoBoardsFromStringList [] 0 (snd numbersDrawnAndRawBoards))

let createGameRecordFromParsedInput (numbersDrawnAndBingoBoards: int list * BingoBoard list): Game =
    {NumbersDrawn = fst numbersDrawnAndBingoBoards; BingoBoards = snd numbersDrawnAndBingoBoards}

let markIfEqual (b: BingoNumber) (value: int) =
    match (b, value) with 
    | (BingoNumber (bValue, false), value) when bValue = value -> BingoNumber(bValue, true)
    | _ -> b

let markNumberInBoard (number: int) (board: BingoBoard) : BingoBoard =
    match board with 
    | BingoBoard bingoNumbers ->
        bingoNumbers
        |> List.map(fun r -> r |> List.map (fun v  -> markIfEqual v number))
        |> BingoBoard

let markNumbersInBoards (number: int) (boards: BingoBoard list) : BingoBoard list =
    boards |> List.map(fun b -> b |> markNumberInBoard number)

let add1IfTrue (b: BingoNumber) =
    match b with 
    | BingoNumber (_, true) -> 1
    | _ ->0

let rec checkBingoColumnWise (rowNumber: int) (board: BingoBoard) : bool =
    let (BingoBoard b) = board 
    let boardArray = b |> array2D
    true
       
let rec checkBingoRowWise (colNumber: int) (board: BingoBoard) : bool = 
    false  

let hasBingo (board: BingoBoard) : bool =
    (checkBingoColumnWise 0 board) || (checkBingoRowWise 0 board) 

let sumOfUnmarkedNumbers (board: BingoBoard) : int = 
    // board |> List.filter(fun v -> List.filter(fun w -> (snd w = true))) |> List.map (fun v -> snd v) |> List.sum
    1

let rec drawNumberAndCheckForWinner (numbersDrawn: int) (game: Game) : BingoBoard option * int option =
    match numbersDrawn >= game.NumbersDrawn.Length with 
    | true -> (None, None) 
    | false -> 
        let markedBoards = markNumbersInBoards (game.NumbersDrawn[numbersDrawn]) game.BingoBoards
        let boardWithBingo = markedBoards |> List.tryFind(fun b -> hasBingo b)
        match boardWithBingo with 
        | Some board -> (Some board, Some game.NumbersDrawn[numbersDrawn])
        | None -> drawNumberAndCheckForWinner (numbersDrawn + 1) {game with BingoBoards = markedBoards}

let determineFirstWinnerAndReturnFinalScore (game: Game) : int = 
    let winningBoardAndNumber = drawNumberAndCheckForWinner 0 game 
    match (fst winningBoardAndNumber, snd winningBoardAndNumber) with 
    | (Some winningBoard, Some winningNumber) -> (sumOfUnmarkedNumbers winningBoard) * winningNumber
    | _ -> -1

let puzzle1 puzzleData =
    puzzleData 
    |> parseNumbersDrawn 
    |> parseBingoBoards 
    |> createGameRecordFromParsedInput
    |> determineFirstWinnerAndReturnFinalScore