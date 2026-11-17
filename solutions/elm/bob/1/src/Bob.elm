module Bob exposing (hey)


hey : String -> String
hey remark =
    let
        answer =
            case String.trim remark of
                "" ->
                    "Fine. Be that way!"

                r ->
                    if (String.toUpper r == r && String.any Char.isAlpha r) && String.endsWith "?" r then
                        "Calm down, I know what I'm doing!"

                    else if String.toUpper r == r && String.any Char.isAlpha r then
                        "Whoa, chill out!"

                    else if String.endsWith "?" r then
                        "Sure."

                    else
                        "Whatever."
    in
    answer
