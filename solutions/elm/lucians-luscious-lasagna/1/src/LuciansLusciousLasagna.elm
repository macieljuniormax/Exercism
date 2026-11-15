module LuciansLusciousLasagna exposing (..)


expectedMinutesInOven : number
expectedMinutesInOven =
    40


preparationTimeInMinutes : number -> number
preparationTimeInMinutes numeroCamadas =
    numeroCamadas * 2


elapsedTimeInMinutes : number -> number -> number
elapsedTimeInMinutes numeroCamadas tempoJaNoForno =
    preparationTimeInMinutes numeroCamadas + tempoJaNoForno
