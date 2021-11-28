open System
open System.Text.RegularExpressions

let validateName name =
    let nameRegex = "^[a-zA-Z ][a-zA-Z ]*$"
    match Regex.IsMatch(name, nameRegex) with 
    | true  -> (true,  name)
    | false -> (false, "")

let validateDate date =
    let dateRegex = "^([0]?[1-9]|[1|2][0-9]|[3][0|1])[./-]([0]?[1-9]|[1][0-2])[./-]([0-9]{4}|[0-9]{2})$"
    match Regex.IsMatch(date, dateRegex) with 
    | true  -> (true,  date)
    | false -> (false, "")

let validateAddress address = 
    let addressRegex = "^[a-zA-Z0-9, ][a-zA-Z0-9, ]*$"
    match Regex.IsMatch(address, addressRegex) with 
    | true -> (true, address)
    | false -> (false, "")

let validateGoals goals = 
    let goalsRegex = "^[a-zA-Z0-9,. ][a-zA-Z0-9,. ]*$"
    match Regex.IsMatch(goals, goalsRegex) with 
    | true -> (true, goals)
    | false -> (false, "")

let printToConsole str =
    printfn "%s" str 

let printToConsoleNoNewLine str =
    printf "%s" str 

let printForm name dob address goals =
    printToConsole $"\nName: {name}\nDate of birth: {dob}\nAddress: {address}\nPersonal goals: {goals}"

let rec readInputFromConsole field validateFunction =
    printToConsoleNoNewLine $"Enter your {field}: "
    let input = Console.ReadLine()
    match validateFunction input with 
    | (false, _) -> 
        printToConsole "Invalid input, try again"
        readInputFromConsole field validateFunction
    | (true, validatedInput) -> validatedInput 

let readNameFromConsole = readInputFromConsole "name" validateName
let readDateOfBirthFromConsole = readInputFromConsole "date of birth" validateDate
let readAddressFromConsole = readInputFromConsole "address" validateAddress
let readGoalsFromConsole = readInputFromConsole "personal goals" validateGoals

[<EntryPoint>]
let main argv =
    printForm readNameFromConsole readDateOfBirthFromConsole readAddressFromConsole readGoalsFromConsole 
    0 // return an integer exit code