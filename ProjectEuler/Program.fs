// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Collections.Generic

// Project Euler Problem 1
let rec sumOfMultiplesOf3And5 n accSum i =
    match i with 
    | _ when i = n -> accSum
    | i when i % 3 = 0 || i % 5 = 0 -> sumOfMultiplesOf3And5 n (accSum + i) (i + 1) 
    | i -> sumOfMultiplesOf3And5 n accSum (i + 1)

let memoise f =
    let dict = Dictionary<_,_>()
    fun c ->
        let exist, value = dict.TryGetValue c 
        match exist with 
        | true -> value 
        | _ -> 
            let value = f c 
            dict.Add(c, value)
            value 

let rec fibonacci n =
    match n with 
    | 0 | 1 -> n
    | n -> fibonacci(n - 1) + fibonacci(n - 2)

let memoFib = memoise fibonacci

[<EntryPoint>]
let main argv =
    printfn "%d" (memoFib 3999999)
    0 // return an integer exit code