open System 

type Rank = | Two = 2 | Three = 3 | Four = 4 | Five = 5 
            | Six = 6 | Seven = 7 | Eight = 8 | Nine = 9 | Ten = 10 
            | Jack = 11 | Queen = 12 | King = 13 | Ace = 14

type Suit = | Spades = 0 | Hearts = 1 | Diamonds = 2 | Clubs = 3

type Card = Rank * Suit 

type Deck = Card list

let createCard rankVal suitVal = 
    Card(rankVal, suitVal)

let printCard (card: Card) =
    match card with 
    | (rank, suit) -> printfn "%A of %A" rank suit

let printCards (cards: Deck) =
    cards |> List.iter (fun card -> printCard card)

let createDeck maxRank numSuits : Deck = 
    let ranks: Rank list = [| for idx in 2 .. maxRank -> enum idx|] |> Array.toList
    let suits: Suit list = [| for idx in 0 .. (numSuits - 1) -> enum idx|] |> Array.toList
    List.allPairs ranks suits 

let shuffle (deck: Deck) = 
    deck |> List.sortBy (fun _ -> (Random()).Next(0, deck.Length - 1))

let dealCard (numOfCardsDealt: int) (deck: Deck) = 
    deck |> List.skip numOfCardsDealt |> List.head

let dealHand (numOfCardsDealt: int) (sizeOfHand: int) (deck: Deck) =
    deck 
    |> List.skip numOfCardsDealt |> List.take sizeOfHand


let rec keepDealing deck =
    deck 
    |> dealHand


[<EntryPoint>]
let main argv =
    let maxRank = 14 
    let numSuits = 4

    let deck = createDeck maxRank numSuits

    0 // return an integer exit code