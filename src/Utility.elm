module Utility exposing (roundTo)


roundTo : Int -> Float -> Float
roundTo k x =
    let
        k_ =
            toFloat k

        factor =
            10.0 ^ k_
    in
    toFloat (round (factor * x)) / factor
