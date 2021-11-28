open System 

type Rank = | Two = 2 | Three = 3 | Four = 4 | Five = 5 
            | Six = 6 | Seven = 7 | Eight = 8 | Nine = 9 | Ten = 10 
            | Jack = 11 | Queen = 12 | King = 13 | Ace = 14

type Suit = | Spades = 0 | Hearts = 1 | Diamonds = 2 | Clubs = 3

type Card = Rank * Suit 

let createCard rankVal suitVal = 
    Card(rankVal, suitVal)

let printCard (card: Card) =
    match card with 
    | (rank, suit) -> printfn "%A of %A" rank suit

let printCards (cards: Card list) =
    cards |> List.iter (fun card -> printCard card)

let createDeck maxRank numSuits : Card list = 
    let ranks: Rank list = [| for idx in 2 .. maxRank -> enum idx|] |> Array.toList
    let suits: Suit list = [| for idx in 0 .. (numSuits - 1) -> enum idx|] |> Array.toList
    List.allPairs ranks suits 

let shuffle (deck: Card list) = 
    /// Warning, side effect here since it randomly shuffles it. input will not be equal to output each time
    let rand = new System.Random()
    deck |> List.sortBy (fun _ -> rand.Next(0, deck.Length - 1))

let dealCard (deck: Card list) (numOfCardsDealt: int)  = 
    deck |> List.skip numOfCardsDealt |> List.head

let dealHand (cards: Card list) (numOfCardsDealt: int) (sizeOfHand: int) =
    cards |> List.skip numOfCardsDealt |> List.take sizeOfHand

let increment count by = 
    count + by 

let parse input = 
    match Int32.TryParse (input: string) with 
    | (true, value) -> value
    | (_, _) -> 0

[<EntryPoint>]
let main argv =
    let maxRank = 14 
    let numSuits = 4
    let handSize = 2
    let numOfPlayers = 4
    let mutable numOfCardsDealt = 0 // Needed to keep track of number of cards dealt
    let deck = createDeck maxRank numSuits |> shuffle

    printf "How many cards do you wish to draw: "
    let input = Console.ReadLine()
    let numOfCardsToDraw = parse input 

    match numOfCardsToDraw with 
    | 0 -> printfn "Invalid argument, zero cards drawn"
    | _ -> dealHand deck numOfCardsDealt numOfCardsToDraw |> printCards

    numOfCardsDealt <- numOfCardsDealt + numOfCardsToDraw
    0 // return an integer exit code