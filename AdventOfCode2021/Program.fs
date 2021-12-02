// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Day1 =
    let readPuzzleInputFromFile file =
        System.IO.File.ReadAllLines(file)
        |> Array.map (fun s ->
            match System.Int32.TryParse s with 
            | (true, i) -> Some i
            | (false, _) -> None)
        |> Array.filter (fun i -> i.IsSome)
        |> Array.map (fun i -> i.Value)
        |> Array.toList

    let isGreater a b = a > b

    let isSumGreater (a: int list) (b: int list) = (a |> List.sum) > (b |> List.sum)

    let rec countMeasurementsGreaterThanLastThreeSlide accCount idx (measurements: int list) = 
        match idx with 
        |  3 ->
            match isSumGreater measurements.[1..3] measurements.[0..2] with 
            | true -> accCount + 1
            | false -> accCount
        | -1 -> measurements |> countMeasurementsGreaterThanLastThreeSlide 0 (measurements.Length - 1) 
        | v  ->
             measurements |>
                match isSumGreater measurements.[v-2..v] measurements.[v-3..v-1] with 
                | true -> countMeasurementsGreaterThanLastThreeSlide (accCount + 1) (idx - 1) 
                | false -> countMeasurementsGreaterThanLastThreeSlide accCount (idx - 1) 
 
    let rec countMeasurementsGreaterThanLast accCount idx (measurements: int list) =
        match idx with 
        |  0 -> accCount
        | -1 -> measurements |> countMeasurementsGreaterThanLast accCount (measurements.Length - 1) 
        |  v ->
            measurements |>
                match isGreater measurements.[v] measurements.[v - 1] with 
                | true -> countMeasurementsGreaterThanLast (accCount + 1) (idx - 1) 
                | false -> countMeasurementsGreaterThanLast accCount (idx - 1) 

    let puzzle1 puzzleData =
        puzzleData |> countMeasurementsGreaterThanLast 0 -1

    let puzzle2 puzzleData =
        puzzleData |> countMeasurementsGreaterThanLastThreeSlide 0 -1


module Day2 =
    type SubInstruction = 
        | Forward of x:int64
        | Down of x:int64 
        | Up of x:int64 

    let parse instructionString : SubInstruction option = 
        let splitInstructionString = System.Text.RegularExpressions.Regex.Split(instructionString, " ") 
        match splitInstructionString.Length with 
        | 2 -> 
            let instructionText = splitInstructionString.[0]
            let instructionValue = System.Int32.Parse splitInstructionString.[1]
            match instructionText with 
            | "forward" ->Some (Forward instructionValue)
            | "up" -> Some (Up instructionValue)
            | "down" -> Some (Down instructionValue)
            | _ -> None 
        | _ -> None

    let rec processInstruction (horizontalPos: int64) (depth: int64)  currIdx (instructions: SubInstruction list) =
        match currIdx with 
        | -1 -> horizontalPos * depth
        | _ ->
            instructions 
            |> match instructions.[currIdx] with 
               | Forward x -> processInstruction (horizontalPos + x) depth (currIdx - 1) 
               | Down x -> processInstruction horizontalPos (depth + x) (currIdx - 1) 
               | Up x -> processInstruction horizontalPos (depth - x) (currIdx - 1)

    let rec processInstructionAim (horizontalPos: int64) (depth: int64) (aim: int64) currIdx (instructions: SubInstruction list) =
        match currIdx with 
        | -1 -> horizontalPos * depth
        | _ ->
            instructions 
            |> match instructions.[currIdx] with 
               | Forward x -> processInstructionAim (horizontalPos + x) (depth + aim*x) aim (currIdx - 1) 
               | Down x -> processInstructionAim horizontalPos depth (aim + x) (currIdx - 1) 
               | Up x -> processInstructionAim horizontalPos depth (aim - x) (currIdx - 1)

    let readPuzzleInputFromFile filePath =
        System.IO.File.ReadAllLines(filePath) 
        |> Array.toList 
        |> List.map (fun s -> parse s)
        |> List.filter (fun s -> s.IsSome)
        |> List.map (fun s -> s.Value)
        |> List.rev

    let puzzle1 (puzzleData: SubInstruction list) =
        processInstruction 0 0 (puzzleData.Length - 1) puzzleData

    let puzzle2 (puzzleData: SubInstruction list) = 
        processInstructionAim 0 0 0 (puzzleData.Length - 1) puzzleData

[<EntryPoint>]
let main argv =
    // let puzzleData = "01/depth_data.txt" |> Day1.readPuzzleInputFromFile
    // Day1.puzzle1 puzzleData |> printfn "Count: %d"
    // Day1.puzzle2 puzzleData |> printfn "Count slide: %d"
    let puzzleData = Day2.readPuzzleInputFromFile "02/instructions.txt"
    Day2.puzzle1 puzzleData |> printfn "Hor pos times depth: %d"
    Day2.puzzle2 puzzleData |> printfn "Hor pos times depth: %d"
    0 // return an integer exit code