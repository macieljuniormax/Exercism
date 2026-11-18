module TopScorers exposing (..)

import Dict exposing (Dict)
import TopScorersSupport exposing (PlayerName)


updateGoalCountForPlayer : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
updateGoalCountForPlayer playerName playerGoalCounts =
    Dict.update playerName
        (\maybeGoals ->
            case maybeGoals of
                Just goals ->
                    Just (goals + 1)

                Nothing ->
                    Just 1
        )
        playerGoalCounts


aggregateScorers : List PlayerName -> Dict PlayerName Int
aggregateScorers playerNames =
    List.foldl
        (\playerName playerGoalsCount -> updateGoalCountForPlayer playerName playerGoalsCount)
        Dict.empty
        playerNames


removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    Dict.filter (\_ goals -> goals >= goalThreshold) playerGoalCounts


resetPlayerGoalCount : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
resetPlayerGoalCount playerName playerGoalCounts =
    Dict.update playerName (\_ -> Just 0) playerGoalCounts


formatPlayer : PlayerName -> Dict PlayerName Int -> String
formatPlayer playerName playerGoalCounts =
    case Dict.get playerName playerGoalCounts of
        Just goals ->
            playerName ++ ": " ++ String.fromInt goals

        Nothing ->
            playerName ++ ": 0"


formatPlayers : Dict PlayerName Int -> String
formatPlayers players =
    Dict.foldl
        (\playerName goals accumulator ->
            if String.isEmpty accumulator then
                playerName ++ ": " ++ String.fromInt goals

            else
                accumulator ++ ", " ++ playerName ++ ": " ++ String.fromInt goals
        )
        ""
        players


combineGames : Dict PlayerName Int -> Dict PlayerName Int -> Dict PlayerName Int
combineGames game1 game2 =
    Dict.merge
        (\playerName goals accumulator -> Dict.insert playerName goals accumulator)
        (\playerName goals1 goals2 accumulator -> Dict.insert playerName (goals1 + goals2) accumulator)
        (\playerName goals accumulator -> Dict.insert playerName goals accumulator)
        game1
        game2
        Dict.empty
