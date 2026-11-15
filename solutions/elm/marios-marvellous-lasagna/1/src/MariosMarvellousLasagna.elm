module MariosMarvellousLasagna exposing (remainingTimeInMinutes)


remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes layers timeSinceBegining =
    let
        expectedMinutesInOven : Int
        expectedMinutesInOven =
            40

        preparationTimeInMinutes : Int -> Int
        preparationTimeInMinutes numberOfLayers =
            numberOfLayers * 2
    in
    preparationTimeInMinutes layers + expectedMinutesInOven - timeSinceBegining
