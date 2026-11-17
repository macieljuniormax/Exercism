module SumOfMultiples exposing (..)

import Set


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    List.concatMap
        (\divisor ->
            List.map
                (\n -> n * divisor)
                (List.range 1 ((limit - 1) // divisor))
        )
        divisors
        |> Set.fromList
        |> Set.toList
        |> List.sum


sumOfMultiples2 : List Int -> Int -> Int
sumOfMultiples2 divisors limit =
    List.sum
        (Set.toList
            (Set.fromList
                (List.concatMap
                    (\divisor ->
                        List.map (\n -> n * divisor) (List.range 1 ((limit - 1) // divisor))
                    )
                    divisors
                )
            )
        )


sumOfMultiples3 : List Int -> Int -> Int
sumOfMultiples3 divisors limit =
    List.sum (listOfUniquesMultiples (listOfAllMultiple divisors limit))


listOfUniquesMultiples : List Int -> List Int
listOfUniquesMultiples allMultiples =
    Set.toList (Set.fromList allMultiples)


listOfAllMultiple : List Int -> Int -> List Int
listOfAllMultiple divisors limit =
    List.concat (List.map (\divisor -> listOfMultiples divisor limit) divisors)


listOfMultiples : Int -> Int -> List Int
listOfMultiples magicItem limit =
    List.map (\n -> n * magicItem) (List.range 1 ((limit - 1) // magicItem))
