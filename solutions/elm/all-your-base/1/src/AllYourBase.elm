module AllYourBase exposing (extractDigits, rebase, toDecimal)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if inBase < 2 || outBase < 2 then
        Nothing

    else if List.isEmpty digits then
        Nothing

    else if not (List.all (\digit -> digit >= 0 && digit < inBase) digits) then
        Nothing

    else if not (List.any (\digit -> digit /= 0) digits) then
        Nothing

    else
        Just
            (fromDecimal
                (toDecimal inBase digits)
                outBase
            )


toDecimal : Int -> List Int -> Int
toDecimal inBase digits =
    List.sum
        (List.indexedMap
            (\index digit -> digit * (inBase ^ index))
            (List.reverse digits)
        )


fromDecimal : Int -> Int -> List Int
fromDecimal decimalDigit outBase =
    if decimalDigit == 0 then
        [ 0 ]

    else
        List.reverse
            (extractDigits decimalDigit outBase)


extractDigits : Int -> Int -> List Int
extractDigits quocient divisor =
    if quocient == 0 then
        []

    else
        let
            rest =
                modBy divisor quocient
        in
        rest :: extractDigits (quocient // divisor) divisor
