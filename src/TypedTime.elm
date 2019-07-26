module TypedTime exposing
    ( HMRecord
    , HMSRecord
    , TypedTime(..)
    , Unit(..)
    , convertFromSecondsWithUnit
    , convertScalarToSecondsWithUnit
    , convertTo
    , convertToSeconds
    , decodeHM
    , decodeMinutes
    , divideBy
    , hmRecordFromSeconds
    , hmStringFromSeconds
    , hmsRecordFromSeconds
    , hmsStringFromSeconds
    , multiply
    , sum
    , timeAsStringWithUnit
    )

{-|

> a = TypedTime Seconds 600
> timeAsStringWithUnit Minutes a
> "00:10" : String

-}

import List.Extra
import Maybe.Extra


type TypedTime
    = TypedTime Unit Float


type Unit
    = Seconds
    | Minutes
    | Hours


sum : List TypedTime -> TypedTime
sum timeList =
    timeList
        |> List.map convertToSeconds
        |> List.sum
        |> TypedTime Seconds


multiply : Float -> TypedTime -> TypedTime
multiply f (TypedTime unit value) =
    TypedTime unit (f * value)


divideBy : TypedTime -> TypedTime -> Float
divideBy denom num =
    convertToSeconds num / convertToSeconds denom


{-|

> convertScalarToSecondsWithUnit Minutes 1
> 60 : Float
> convertScalarToSecondsWithUnit Hours 1
> 3600 : Float
> convertScalarToSecondsWithUnit Seconds 1
> 1 : Float

-}
convertScalarToSecondsWithUnit : Unit -> Float -> Float
convertScalarToSecondsWithUnit inputUnit scalar =
    let
        inputValue =
            TypedTime inputUnit scalar
    in
    convertToSeconds inputValue


convertToSeconds : TypedTime -> Float
convertToSeconds tt =
    case tt of
        TypedTime Seconds s ->
            s

        TypedTime Minutes m ->
            60 * m

        TypedTime Hours h ->
            3600 * h


convertFromSecondsWithUnit : Unit -> Float -> TypedTime
convertFromSecondsWithUnit unit s =
    case unit of
        Seconds ->
            TypedTime Seconds s

        Minutes ->
            TypedTime Minutes (s / 60)

        Hours ->
            TypedTime Hours (s / 3600)


{-|

> convertTo Hours a
> TypedTime Hours 0.16666666666666666 : TypedTime

-}
convertTo : Unit -> TypedTime -> TypedTime
convertTo unit tt =
    convertToSeconds tt
        |> convertFromSecondsWithUnit unit


timeAsStringWithUnit : Unit -> TypedTime -> String
timeAsStringWithUnit unit tt =
    let
        (TypedTime unit_ value) =
            convertTo Seconds tt
    in
    case unit of
        Seconds ->
            hmsStringFromSeconds value

        Minutes ->
            hmStringFromSeconds value

        Hours ->
            hmStringFromSeconds value


type alias HMSRecord =
    { seconds : Int, minutes : Int, hours : Int }


type alias HMRecord =
    { hours : Int, minutes : Int }


hmsRecordFromSeconds : Float -> HMSRecord
hmsRecordFromSeconds s =
    let
        s1 =
            round s

        s2 =
            modBy 60 s1

        m1 =
            s1 // 60

        m2 =
            modBy 60 m1

        h1 =
            m1 // 60
    in
    { seconds = s2, minutes = m2, hours = h1 }


hmsStringFromSeconds : Float -> String
hmsStringFromSeconds s =
    let
        tr =
            hmsRecordFromSeconds s
    in
    (String.padLeft 2 '0' <| String.fromInt tr.hours)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.minutes)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.seconds)



---
---


hmRecordFromSeconds : Float -> HMRecord
hmRecordFromSeconds s =
    let
        m1 =
            round (s / 60)

        m2 =
            modBy 60 m1

        h1 =
            m1 // 60
    in
    { minutes = m2, hours = h1 }


hmStringFromSeconds : Float -> String
hmStringFromSeconds s =
    let
        tr =
            hmRecordFromSeconds s
    in
    (String.padLeft 2 '0' <| String.fromInt tr.hours)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.minutes)



--
-- TIME PARSER
--


{-| -}
decodeHM : String -> Maybe Float
decodeHM str =
    let
        parts =
            String.split ":" (String.trim str)
                |> List.map String.toFloat
                |> Maybe.Extra.values
    in
    case List.length parts of
        1 ->
            Maybe.map (\x -> 60 * x) (List.Extra.getAt 0 parts)

        2 ->
            let
                hoursPart =
                    Maybe.map (\x -> 3600 * x) (List.Extra.getAt 0 parts)

                minutesPart =
                    Maybe.map (\x -> 60 * x) (List.Extra.getAt 1 parts)
            in
            Maybe.map2 (+) hoursPart minutesPart

        _ ->
            Nothing


decodeMinutes : String -> TypedTime
decodeMinutes str =
    case decodeHM str of
        Nothing ->
            TypedTime Seconds 0

        Just t ->
            TypedTime Seconds (60.0 * t)
