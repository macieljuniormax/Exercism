module Allergies exposing (Allergy(..), isAllergicTo, toList)

import Bitwise


type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


allergies : List Allergy
allergies =
    [ Eggs
    , Peanuts
    , Shellfish
    , Strawberries
    , Tomatoes
    , Chocolate
    , Pollen
    , Cats
    ]


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    case allergy of
        Eggs ->
            Bitwise.and score 1 == 1

        Peanuts ->
            Bitwise.and score 2 == 2

        Shellfish ->
            Bitwise.and score 4 == 4

        Strawberries ->
            Bitwise.and score 8 == 8

        Tomatoes ->
            Bitwise.and score 16 == 16

        Chocolate ->
            Bitwise.and score 32 == 32

        Pollen ->
            Bitwise.and score 64 == 64

        Cats ->
            Bitwise.and score 128 == 128


toList : Int -> List Allergy
toList score =
    List.filter (\allergy -> isAllergicTo allergy score) allergies
