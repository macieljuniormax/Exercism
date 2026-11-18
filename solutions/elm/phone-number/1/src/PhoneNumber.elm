module PhoneNumber exposing (..)


getNumber : String -> Maybe String
getNumber phoneNumber =
    let
        verifiedPhoneNumber =
            phoneNumber
                |> cleanPhoneNumber
                |> normalizeNumber
                |> isValidNumber
    in
    if String.isEmpty verifiedPhoneNumber then
        Nothing

    else
        Just verifiedPhoneNumber


cleanPhoneNumber : String -> String
cleanPhoneNumber phoneNumber =
    String.filter Char.isDigit phoneNumber


normalizeNumber : String -> String
normalizeNumber phoneNumber =
    if String.length phoneNumber == 10 || String.length phoneNumber == 11 then
        if (String.length phoneNumber == 11) && String.startsWith "1" phoneNumber then
            String.dropLeft 1 phoneNumber

        else if String.length phoneNumber == 10 then
            phoneNumber

        else
            ""

    else
        ""


isValidNumber : String -> String
isValidNumber phoneNumber =
    let
        firstDigit : String
        firstDigit =
            String.slice 0 1 phoneNumber

        fourthDigit : String
        fourthDigit =
            String.slice 3 4 phoneNumber

        isValidDigit : String -> Bool
        isValidDigit digit =
            digit >= "2" && digit <= "9"
    in
    if isValidDigit firstDigit && isValidDigit fourthDigit then
        phoneNumber

    else
        ""
