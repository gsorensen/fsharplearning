module Day1 

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