module DateTime exposing
    ( NaiveDateTime(..)
    , dateStringOfDateTimeString
    , humanDateStringFromPosix
    , isoStringFromNaiveDateTime
    , julianDayNumber
    , naiveDateStringFromPosix
    , naiveDateTimeFromPosix
    , naiveDateTimeValue
    , naiveTimeStringFromPosix
    , rataDieFromPosix
    , timeStringOfDateTimeString
    )

-- import Date exposing (Date, Unit(..), diff)
-- import Svg.Attributes exposing (targetY)

import Time exposing (Posix)


type NaiveDateTime
    = NaiveDateTime String


type alias TR =
    { hour : Int
    , minute : Int
    , second : Int
    }


naiveDateTimeValue : NaiveDateTime -> String
naiveDateTimeValue (NaiveDateTime str) =
    str


offsetTRByHours : Int -> TR -> TR
offsetTRByHours h tt =
    let
        newHours =
            tt.hour + h
    in
    if newHours >= 24 then
        { tt | hour = newHours - 24 }

    else if newHours > 0 then
        { tt | hour = newHours }

    else
        { tt | hour = newHours + 24 }


stringValueOfTR : TR -> String
stringValueOfTR tr =
    let
        h =
            String.padLeft 2 '0' <| String.fromInt tr.hour

        min =
            String.padLeft 2 '0' <| String.fromInt tr.minute

        s =
            String.padLeft 2 '0' <| String.fromInt tr.second
    in
    String.join ":" [ h, min, s ]


strToInt : String -> Int
strToInt str =
    str
        |> String.toInt
        |> Maybe.withDefault 0


isoStringFromNaiveDateTime : String -> Maybe String
isoStringFromNaiveDateTime str =
    String.split "T" str
        |> List.head


naiveDateStringFromPosix : Posix -> String
naiveDateStringFromPosix posix =
    let
        y =
            Time.toYear Time.utc posix |> String.fromInt

        m =
            Time.toMonth Time.utc posix |> monthToStringAsNumber

        d =
            Time.toDay Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    in
    y ++ "-" ++ m ++ "-" ++ d


humanDateStringFromPosix : Posix -> String
humanDateStringFromPosix posix =
    let
        y =
            Time.toYear Time.utc posix |> String.fromInt

        m =
            Time.toMonth Time.utc posix |> monthToString

        d =
            Time.toDay Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    in
    d ++ " " ++ m ++ " " ++ y


naiveTimeStringFromPosix : Posix -> String
naiveTimeStringFromPosix posix =
    let
        h =
            Time.toHour Time.utc posix |> String.fromInt |> String.padLeft 2 '0'

        m =
            Time.toMinute Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    in
    h ++ ":" ++ m


naiveDateTimeFromPosix : Posix -> NaiveDateTime
naiveDateTimeFromPosix posix =
    NaiveDateTime (naiveDateStringFromPosix posix)


monthToStringAsNumber : Time.Month -> String
monthToStringAsNumber m =
    case m of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


monthToString : Time.Month -> String
monthToString m =
    case m of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


timeStringOfDateTimeString : String -> String
timeStringOfDateTimeString str =
    case str == "1900-01-01" of
        True ->
            ""

        False ->
            str
                |> String.split "T"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault "-"


dateStringOfDateTimeString : String -> String
dateStringOfDateTimeString str =
    case str == "1900-01-01" of
        True ->
            ""

        False ->
            str
                |> String.split "T"
                |> List.head
                |> Maybe.withDefault "-"


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


julianDayNumber : Posix -> Float
julianDayNumber posix =
    let
        y =
            Time.toYear Time.utc posix |> toFloat

        m =
            Time.toMonth Time.utc posix |> monthToInt |> toFloat

        d =
            Time.toDay Time.utc posix |> toFloat
    in
    (1461 * (y + 4800 + (m - 14) / 12)) / 4 + (367 * (m - 2 - 12 * ((m - 14) / 12))) / 12 - (3 * ((y + 4900 + (m - 14) / 12) / 100)) / 4 + d - 32075


rataDieFromPosix : Posix -> Int
rataDieFromPosix posix =
    floor (julianDayNumber posix - 1721424.5)



-- https://www.revolvy.com/page/Rata-Die
-- https://www.revolvy.com/page/Julian-day?cr=1
-- RD = floor( JD − 1 721 424.5 )
