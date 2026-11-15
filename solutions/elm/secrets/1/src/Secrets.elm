module Secrets exposing (clearBits, decrypt, flipBits, setBits, shiftBack)

import Bitwise


shiftBack : Int -> Int -> Int
shiftBack amount value =
    Bitwise.shiftRightZfBy amount value


setBits : Int -> Int -> Int
setBits mask value =
    Bitwise.or mask value


flipBits : Int -> Int -> Int
flipBits mask value =
    Bitwise.xor mask value


clearBits : Int -> Int -> Int
clearBits mask value =
    Bitwise.and (Bitwise.complement mask) value


decrypt : Int -> Int
decrypt secret =
    let
        setedBits : Int
        setedBits =
            setBits 1996 secret

        flippedBits : Int
        flippedBits =
            flipBits 2009 setedBits

        shiftedBits : Int
        shiftedBits =
            shiftBack 5 flippedBits

        clearedBits : Int
        clearedBits =
            clearBits 17 shiftedBits

        decryptSecret : Int
        decryptSecret =
            clearedBits
    in
    decryptSecret
