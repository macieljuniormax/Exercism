module ComplexNumbers exposing
    ( Complex
    , abs
    , add
    , conjugate
    , div
    , exp
    , fromPair
    , fromReal
    , imaginary
    , mul
    , real
    , sub
    )

import Time exposing (Zone)


type Complex
    = Complex Float Float


fromPair : ( Float, Float ) -> Complex
fromPair pair =
    let
        ( a, b ) =
            pair
    in
    Complex a b


fromReal : Float -> Complex
fromReal float =
    Complex float 0


real : Complex -> Float
real z =
    case z of
        Complex a _ ->
            a


imaginary : Complex -> Float
imaginary z =
    case z of
        Complex _ b ->
            b


conjugate : Complex -> Complex
conjugate z =
    let
        a =
            real z

        b =
            imaginary z
    in
    Complex a -b


abs : Complex -> Float
abs z =
    let
        a =
            real z

        b =
            imaginary z
    in
    sqrt ((a ^ 2) + (b ^ 2))


add : Complex -> Complex -> Complex
add z1 z2 =
    let
        a =
            real z1

        b =
            imaginary z1

        c =
            real z2

        d =
            imaginary z2
    in
    Complex (a + c) (b + d)


sub : Complex -> Complex -> Complex
sub z1 z2 =
    let
        a =
            real z1

        b =
            imaginary z1

        c =
            real z2

        d =
            imaginary z2
    in
    Complex (a - c) (b - d)


mul : Complex -> Complex -> Complex
mul z1 z2 =
    let
        a =
            real z1

        b =
            imaginary z1

        c =
            real z2

        d =
            imaginary z2
    in
    Complex (a * c - b * d) (b * c + a * d)


div : Complex -> Complex -> Complex
div z1 z2 =
    let
        a =
            real z1

        b =
            imaginary z1

        c =
            real z2

        d =
            imaginary z2
    in
    Complex ((a * c + b * d) / (c ^ 2 + d ^ 2)) ((b * c - a * d) / (c ^ 2 + d ^ 2))


exp : Complex -> Complex
exp z =
    let
        a =
            real z

        b =
            imaginary z
    in
    Complex (Basics.e ^ a * cos b) (Basics.e ^ a * sin b)
