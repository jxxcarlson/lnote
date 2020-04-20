module Utility exposing (randomIntegers, currentUserIsAdmin, roundTo)

import Types exposing (FrontendModel)
import Random

type alias Model = FrontendModel



currentUserIsAdmin : Model -> Bool
currentUserIsAdmin model =
    case model.currentUser of
        Nothing ->
            False

        Just user ->
            user.admin

roundTo : Int -> Float -> Float
roundTo k x =
    let
        k_ =
            toFloat k

        factor =
            10.0 ^ k_
    in
    toFloat (round (factor * x)) / factor


{-| Generate a list of length k of integers in the range 0..n
-}
randomIntegers : Int -> Int -> Random.Generator (List Int)
randomIntegers k n =
    Random.list k (Random.int 0 n)
