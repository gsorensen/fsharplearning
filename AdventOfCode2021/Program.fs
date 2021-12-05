// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open Day1
open Day2
open Day3 
open Day4

[<EntryPoint>]
let main argv =
    // let puzzleData = "03/diagnostics_report.txt" |> Day3.readPuzzleInputFromFile 
    // Day3.puzzle1 puzzleData |> printfn "%d"
    // Day3.puzzle2 puzzleData |> printfn "%d"
    // let puzzleData = readPuzzleInputFromFile "04/puzzle_input.txt" 
    let puzzleData = readPuzzleInputFromFile "04/test_input.txt" 
    Day4.puzzle1 puzzleData |> printfn "%d"

    0