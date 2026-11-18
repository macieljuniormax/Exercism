module Go exposing (..)

import GoSupport exposing (..)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    case oneStonePerPointRule game of
        Err errorMessage ->
            { game | error = errorMessage }

        Ok gameAfterFirstRule ->
            let
                gameAfterCapture =
                    captureRule gameAfterFirstRule
            in
            case libertyRule gameAfterCapture of
                Err errorMessage ->
                    { game | error = errorMessage }

                Ok gameAfterLiberty ->
                    case koRule gameAfterLiberty of
                        Err errorMessage ->
                            { game | error = errorMessage }

                        Ok finalGame ->
                            changePlayer finalGame
