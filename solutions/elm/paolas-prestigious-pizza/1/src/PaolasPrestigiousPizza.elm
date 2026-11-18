module PaolasPrestigiousPizza exposing
    ( Pizza
    , ingredientsParser
    , menuParser
    , oneIngredientParser
    , pizzaParser
    , priceParser
    , vegetarianParser
    , wordParser
    )

import Parser exposing ((|.), (|=), Parser)


type alias Pizza =
    { name : String
    , vegetarian : Bool
    , ingredients : List String
    , price : Int
    }


priceParser : Parser Int
priceParser =
    Parser.succeed identity
        |= Parser.int
        |. Parser.symbol "€"


vegetarianParser : Parser Bool
vegetarianParser =
    Parser.oneOf
        [ Parser.symbol "(v)" |> Parser.map (\_ -> True)
        , Parser.succeed False
        ]


wordParser : Parser String
wordParser =
    Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString
        |> Parser.map String.toLower


oneIngredientParser : Parser String
oneIngredientParser =
    Parser.chompWhile
        (\c ->
            (c >= 'a' && c <= 'z')
                || (c >= 'A' && c <= 'Z')
                || c
                == ' '
        )
        |> Parser.getChompedString
        |> Parser.andThen
            (\ingredient ->
                let
                    cleanedIngredient =
                        ingredient
                            |> String.trim
                            |> String.toLower
                in
                if String.isEmpty cleanedIngredient then
                    Parser.problem "empty string"

                else
                    Parser.succeed cleanedIngredient
            )


ingredientsParser : Parser (List String)
ingredientsParser =
    Parser.oneOf
        [ Parser.sequence
            { start = ""
            , separator = ","
            , end = ""
            , spaces = Parser.spaces
            , item = oneIngredientParser
            , trailing = Parser.Forbidden
            }
        , Parser.succeed []
        ]


pizzaParser : Parser Pizza
pizzaParser =
    Parser.succeed Pizza
        |= wordParser
        |. Parser.spaces
        |= vegetarianParser
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces
        |= ingredientsParser
        |. Parser.spaces
        |. Parser.symbol "-"
        |. Parser.spaces
        |= priceParser


menuParser : Parser (List Pizza)
menuParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.succeed ()
        , item = pizzaParser
        , trailing = Parser.Optional
        }
        |. Parser.end
