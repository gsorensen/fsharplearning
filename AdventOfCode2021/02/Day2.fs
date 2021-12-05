module Day2 

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