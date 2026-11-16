module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)


clean1 : String -> String
clean1 str =
    str
        |> String.replace " " "_"


clean2 : String -> String
clean2 str =
    str
        |> clean1
        |> String.replace "\n" "[CTRL]"
        |> String.replace "\t" "[CTRL]"
        |> String.replace "\u{000D}" "[CTRL]"


clean3 : String -> String
clean3 str =
    str
        |> clean2
        |> String.toList
        |> cleanKebab
        |> String.fromList


clean4 : String -> String
clean4 str =
    str
        |> clean3
        |> String.filter (\char -> not (Char.isDigit char))


clean : String -> String
clean str =
    str
        |> clean4
        |> String.filter (\char -> not (char >= 'α' && char <= 'ω'))


cleanKebab : List Char -> List Char
cleanKebab chars =
    case chars of
        [] ->
            []

        current :: next :: rest ->
            if current == '-' then
                Char.toUpper next :: cleanKebab rest

            else
                current :: cleanKebab (next :: rest)

        current :: rest ->
            current :: cleanKebab rest
