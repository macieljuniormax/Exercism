module MagicianInTraining exposing (..)

import Array exposing (Array)


getCard : Int -> Array Int -> Maybe Int
getCard index deck =
    Array.get index deck


setCard : Int -> Int -> Array Int -> Array Int
setCard index newCard deck =
    Array.set index newCard deck


addCard : Int -> Array Int -> Array Int
addCard newCard deck =
    Array.push newCard deck


removeCard : Int -> Array Int -> Array Int
removeCard index deck =
    if index < 0 || index >= Array.length deck then
        deck

    else
        Array.append
            (Array.slice 0 index deck)
            (Array.slice (index + 1) (Array.length deck) deck)


evenCardCount : Array Int -> Int
evenCardCount deck =
    Array.foldl
        (\card accumulator ->
            if modBy 2 card == 0 then
                accumulator + 1

            else
                accumulator
        )
        0
        deck
