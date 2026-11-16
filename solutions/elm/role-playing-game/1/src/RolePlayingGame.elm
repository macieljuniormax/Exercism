module RolePlayingGame exposing (Player, castSpell, introduce, revive)

import Html.Attributes exposing (name)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    case name of
        Nothing ->
            "Mighty Magician"

        Just someName ->
            someName


revive : Player -> Maybe Player
revive player =
    if player.health > 0 then
        Nothing

    else
        Just
            { player
                | health = 100
                , mana =
                    if player.level >= 10 then
                        Just 100

                    else
                        Nothing
            }


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Nothing ->
            ( { player
                | health = max 0 (player.health - manaCost)
              }
            , 0
            )

        Just currentMana ->
            if currentMana >= manaCost then
                ( { player
                    | mana = Just (currentMana - manaCost)
                  }
                , manaCost * 2
                )

            else
                ( player, 0 )
