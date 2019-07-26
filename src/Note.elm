module Note exposing (Note, bigDateFilter, filter, kDaysAgo, replace)

import List.Extra
import Time exposing (Posix)


type alias Note =
    { id : Int
    , subject : String
    , tags : List String
    , body : String
    , timeCreated : Posix
    , timeModified : Posix
    }


replace : Note -> List Note -> List Note
replace note noteList =
    List.Extra.setIf (\n -> n.id == note.id) note noteList



-- filter : String -> List Note -> List Note
-- filter str noteList =
--     noteList


bigDateFilter : Posix -> String -> String -> List Note -> List Note
bigDateFilter today prefixParameterString suffixParameterString noteList =
    noteList
        |> prefixFilter today prefixParameterString
        |> suffixFilter today suffixParameterString


suffixFilter : Posix -> String -> List Note -> List Note
suffixFilter today suffixParameterString noteList =
    case suffixParameterString |> String.toInt of
        Nothing ->
            noteList

        Just k ->
            dateSuffixFilter today k noteList


prefixFilter : Posix -> String -> List Note -> List Note
prefixFilter today prefixParameterString noteList =
    case prefixParameterString |> String.toInt of
        Nothing ->
            noteList

        Just k ->
            datePrefixFilter today k noteList


filter : String -> List Note -> List Note
filter filterString notes =
    List.filter (\note -> String.contains (String.toLower filterString) (String.toLower note.subject)) notes


dateSuffixFilter : Posix -> Int -> List Note -> List Note
dateSuffixFilter today k noteList =
    let
        kDaysAgo_ =
            kDaysAgo k today
    in
    List.filter (\note -> posixInterval note.timeCreated kDaysAgo_ >= 0) noteList


datePrefixFilter : Posix -> Int -> List Note -> List Note
datePrefixFilter today k noteList =
    let
        kDaysAgo_ =
            kDaysAgo k today
    in
    List.filter (\note -> posixInterval kDaysAgo_ note.timeCreated >= -1) noteList


shiftPosix : Float -> Posix -> Posix
shiftPosix t p =
    ((Time.posixToMillis p |> toFloat) + (1000.0 * t))
        |> round
        |> Time.millisToPosix


kDaysAgo : Int -> Posix -> Posix
kDaysAgo k posix =
    shiftPosix (-86400.0 * toFloat k) posix


{-| Interval betwen two Posix times in Seconds
-}
posixInterval : Posix -> Posix -> Float
posixInterval p_ q_ =
    let
        p =
            Time.posixToMillis p_ |> toFloat

        q =
            Time.posixToMillis q_ |> toFloat
    in
    (p - q) / 1000.0
