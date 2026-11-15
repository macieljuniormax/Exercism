module DifferenceOfSquares exposing (difference, squareOfSum, sumOfSquares)

import List exposing (sum)


squareOfSum : Int -> Int
squareOfSum n =
    let
        sum : Int
        sum =
            ((1 + n) * n) // 2
    in
    sum ^ 2


sumOfSquares : Int -> Int
sumOfSquares n =
    ((n * (n + 1)) * ((2 * n) + 1)) // 6


difference : Int -> Int
difference n =
    squareOfSum n - sumOfSquares n
