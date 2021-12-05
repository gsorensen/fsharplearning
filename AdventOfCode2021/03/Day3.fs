module Day3 

type DiagnosticData = DiagnosticData of binaryString: string

let value (s: char) = System.Int32.Parse (string s)

let readPuzzleInputFromFile filePath = 
    System.IO.File.ReadAllLines(filePath) 
    |> Array.toList
    |> List.map (fun s -> Seq.toList s |> List.map (fun s -> value s))

let greaterThanOrEqual a b = a >= b 
let lessThan a b = a < b

let isGreaterThanOrEqualBinary a b = 
    match a >= (b / 2) with 
    | true -> 1
    | false -> 0

let binaryFlip a = 
    match a with 
    | 1 -> 0 
    | _ -> 1

let gammaEpsilonRate accListSum numOfMeasurements = 
    let gammaRate = accListSum |> List.map(fun s -> isGreaterThanOrEqualBinary s numOfMeasurements)
    let epsilonRate = gammaRate |> List.map(fun s -> binaryFlip s)
    (gammaRate, epsilonRate)

let rec sumListColumnWise (accListSum: int list) (diagnosticsList: int list list) currIdx =
    match (currIdx, diagnosticsList.Length) with 
    | (c, d) when c = d -> accListSum 
    | _ -> 
        let newAccListSum = List.map2 (fun x y -> x + y) accListSum diagnosticsList[currIdx]
        sumListColumnWise newAccListSum diagnosticsList (currIdx + 1)

let getBinaryStringFromIntList (s: int list) = 
    s |> List.map(fun s -> s.ToString()) |> List.fold (fun str s -> str + s) ""

let isBitEqual (a:int) (b:int) = 
    a = b

let rec checkRating (mostCommonBits: int list)  (measurement: int list) idx =
        let measurementLength = mostCommonBits.Length
        match (idx, measurementLength - 1) with
        | (currIdx, maxIdx) when currIdx = maxIdx -> mostCommonBits[maxIdx] = measurement[maxIdx]
        | (currIdx, _) when mostCommonBits[currIdx] = measurement[currIdx] -> checkRating mostCommonBits  measurement (idx + 1)
        | _ -> false 

let puzzle1 (puzzleData: int list list) = 
    let accListSumStart = puzzleData[0] |> List.map(fun s -> s * 0)
    let accListSum = sumListColumnWise accListSumStart puzzleData 0 
    let gammaAndEpsilonRate = gammaEpsilonRate accListSum puzzleData.Length
    let gammaRateBinaryString = getBinaryStringFromIntList (fst gammaAndEpsilonRate)
    let epsilonRateBinaryString = getBinaryStringFromIntList (snd gammaAndEpsilonRate)
    let gammaRate = System.Convert.ToInt32(gammaRateBinaryString, 2)
    let epsilonRate = System.Convert.ToInt32(epsilonRateBinaryString, 2)
    gammaRate * epsilonRate

let findMostCommonBitInEachPosition (comparator: int -> int -> bool) (data: int list list) : int list =
    data
    |> List.reduce (List.map2 (+))  
    |> List.map (fun v -> match comparator v (data.Length/2) with | true -> 1 | false -> 0)

let findLeastCommonBitInEachPosition (comparator: int -> int -> bool) (data: int list list) : int list =
    data 
    |> List.reduce (List.map2 (+))
    |> List.map (fun v -> match comparator v (data.Length/2) with | true -> 1 | false -> 0)

let rec determineRatingRec (idx: int) (bits: int list) (measurements: int list list) = 
    match (idx = bits.Length || measurements.Length = 1) with 
    | true ->  measurements[0]
    | false -> 
        measurements
        |> List.filter (fun v -> v[idx] = bits[idx])
        |> determineRatingRec (idx + 1) bits 

let getDecimalValueFromBinaryIntList (l: int list) = 
    let binaryString = l |> List.map(fun s -> s.ToString()) |> List.fold (fun str s -> str + s) ""
    System.Convert.ToInt32(binaryString, 2)

let determineOxygenRating (measurements: int list list) (msb: int list)  = 
    measurements
    |> determineRatingRec 0 msb  

let determineCO2ScrubberRating (measurements: int list list) (lsb: int list) =
    measurements 
    |> determineRatingRec 0 lsb 

let oxygenRating (measurements: int list list) = 
    measurements 
    |> findMostCommonBitInEachPosition greaterThanOrEqual
    |> determineOxygenRating measurements
    |> getDecimalValueFromBinaryIntList

let co2ScrubberRating (measurements: int list list) =
    measurements 
    |> findMostCommonBitInEachPosition greaterThanOrEqual
    |> List.map (fun b -> binaryFlip b)
    |> determineCO2ScrubberRating measurements 
    |> getDecimalValueFromBinaryIntList

let puzzle2 (puzzleData: int list list) = 
    let subOxygenRating = oxygenRating puzzleData
    let subCO2ScrubberRating = co2ScrubberRating puzzleData
    subOxygenRating * subCO2ScrubberRating