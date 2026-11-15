module SumOfMultiples exposing (sumOfMultiples)

import Set


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
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



-- sumOfMultiples : List Int -> Int -> Int
-- sumOfMultiples divisors limit =
--     List.sum (listOfUniquesMultiples (listOfAllMultiple divisors limit))
-- listOfUniquesMultiples : List Int -> List Int
-- listOfUniquesMultiples allMultiples =
--     Set.toList (Set.fromList allMultiples)
-- listOfAllMultiple : List Int -> Int -> List Int
-- listOfAllMultiple divisors limit =
--     List.concat (List.map (\divisor -> listOfMultiples divisor limit) divisors)
-- listOfMultiples : Int -> Int -> List Int
-- listOfMultiples magicItem limit =
--     List.map (\n -> n * magicItem) (List.range 1 ((limit - 1) // magicItem))
