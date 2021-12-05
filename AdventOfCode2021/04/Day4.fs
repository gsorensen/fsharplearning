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
    |> Array.toList
    |> List.map (fun s -> intFrom s)
    |> List.filter (fun v -> v.IsSome)
    |> List.map (fun v -> v.Value)
    |> List.map (fun v -> BingoNumber(v, false))

let bingoBoardFromStringList (rawInput: string list) = 
    rawInput 
    |> List.map (fun s -> bingoNumbersFromString s)

let rec bingoBoardsFromStringList (bingoBoards: BingoBoard list) (idx: int) (rawData: string list) : BingoBoard list =
    let newBingoBoard = rawData[idx+1..idx+5] |> bingoBoardFromStringList |> BingoBoard
    match idx+6 > rawData.Length with 
    | true -> (newBingoBoard::bingoBoards).Tail
    | false -> bingoBoardsFromStringList (newBingoBoard::bingoBoards) (idx+6) rawData

let parseBingoBoards (numbersDrawnAndRawBoards: int list * string list)  : int list * BingoBoard list =
    (fst numbersDrawnAndRawBoards, bingoBoardsFromStringList [] 0 (snd numbersDrawnAndRawBoards))

let createGameRecordFromParsedInput (numbersDrawnAndBingoBoards: int list * BingoBoard list): Game =
    {NumbersDrawn = fst numbersDrawnAndBingoBoards; BingoBoards = snd numbersDrawnAndBingoBoards}

let determineFirstWinnerAndReturnFinalScore (game: Game) : int = 
    0

let puzzle1 puzzleData =
    puzzleData 
    |> parseNumbersDrawn 
    |> parseBingoBoards 
    |> createGameRecordFromParsedInput
    |> determineFirstWinnerAndReturnFinalScore