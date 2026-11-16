module Diamond exposing (rows)


rows : Char -> String
rows letter =
    let
        size =
            (2 * (Char.toCode letter - 64)) - 1

        matrix : List (List Char)
        matrix =
            List.repeat size (List.repeat size '_')
    in
    fillDiamond matrix letter
        |> List.map String.fromList
        |> String.join "\n"


fillDiamond : List (List Char) -> Char -> List (List Char)
fillDiamond matrix letter =
    let
        size =
            List.length matrix

        center =
            (size - 1) // 2
    in
    List.indexedMap
        (\rowIndex row ->
            let
                distance =
                    abs (rowIndex - center)

                char =
                    Char.fromCode (Char.toCode letter - distance)

                left =
                    distance

                right =
                    size - 1 - distance
            in
            List.indexedMap
                (\columnIndex current ->
                    if columnIndex == left || columnIndex == right then
                        char

                    else
                        current
                )
                row
        )
        matrix
