namespace NumberGame 

module Types =
    type Answer = Answer of int 
    type Guess = Guess of int
    type Bounds = Bounds of int * int
    type Round = Guess * Answer * Bounds
    type RoundResult = | BelowLowerBound | BelowAnswer | Correct | AboveUpperBound | AboveAnswer 

