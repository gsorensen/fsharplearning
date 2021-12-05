// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open Day1
open Day2
open Day3 

[<EntryPoint>]
let main argv =
    let puzzleData = "03/diagnostics_report.txt" |> Day3.readPuzzleInputFromFile 
    Day3.puzzle1 puzzleData |> printfn "%d"
    Day3.puzzle2 puzzleData |> printfn "%d"
    0 // return an integer exit code