module Note exposing
    ( Note
    , applyBodyFilter
    , applySubjectFilter
    , applyTagFilter
    , bigDateFilter
    , filter
    , filterByTag
    , filterText
    , firstSelectedNote
    , frequencies
    , kDaysAgo
    , make
    , remove
    , replace
    , select
    , selectAll
    , tagsFromString
    )

import FrequencyDict exposing (FrequencyDict)
import List.Extra
import Time exposing (Posix)


type alias Note =
    { id : Int
    , subject : String
    , tags : List String
    , body : String
    , timeCreated : Posix
    , timeModified : Posix
    , selected : Bool
    }


make : Int -> String -> String -> Posix -> Note
make id subject body posix =
    { id = id
    , subject = subject
    , tags = []
    , body = body
    , timeCreated = posix
    , timeModified = posix
    , selected = True
    }


replace : Note -> List Note -> List Note
replace note noteList =
    case List.member note.id (noteList |> List.map .id) of
        True ->
            List.Extra.setIf (\n -> n.id == note.id) note noteList

        False ->
            note :: noteList


remove : Note -> List Note -> List Note
remove note noteList =
    List.filter (\n -> n.id /= note.id) noteList


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


filterText : String -> List Note -> List Note
filterText filterString notes =
    case filterString of
        "" ->
            notes

        _ ->
            let
                textFilter str note =
                    String.contains (String.toLower str) (String.toLower note.body)
            in
            List.filter (\note -> textFilter filterString note) notes


filterByTag : String -> List Note -> List Note
filterByTag tag notes =
    case tag of
        "" ->
            notes

        _ ->
            let
                tagFilter tag_ note =
                    List.member (String.toLower tag_) note.tags
            in
            List.filter (\note -> tagFilter tag note) notes


dateSuffixFilter : Posix -> Int -> List Note -> List Note
dateSuffixFilter today k noteList =
    let
        kDaysAgo_ =
            kDaysAgo k today

        f : Note -> Bool
        f note =
            posixInterval note.timeCreated kDaysAgo_ >= 0

        select_ : Note -> Note
        select_ note =
            { note | selected = f note }
    in
    List.map select_ noteList


datePrefixFilter : Posix -> Int -> List Note -> List Note
datePrefixFilter today k noteList =
    let
        kDaysAgo_ =
            kDaysAgo k today

        f : Note -> Bool
        f note =
            posixInterval kDaysAgo_ note.timeCreated >= -1

        select_ : Note -> Note
        select_ note =
            { note | selected = f note }
    in
    List.map select_ noteList


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


tagsFromString : String -> List String
tagsFromString str =
    str
        |> String.split ","
        |> List.map String.trim


frequencies : List Note -> FrequencyDict
frequencies noteList =
    FrequencyDict.make (List.map .tags noteList |> List.concat)


selectAll : List Note -> List Note
selectAll noteList =
    noteList
        |> List.map (\note -> { note | selected = True })


select : Note -> Note
select note =
    { note | selected = True }


applySubjectFilter : String -> List Note -> List Note
applySubjectFilter str noteList =
    let
        f : Note -> Bool
        f note =
            String.contains (String.toLower str) (String.toLower note.subject)

        select_ : Note -> Note
        select_ note =
            { note | selected = f note }
    in
    List.map select_ noteList


applyBodyFilter : String -> List Note -> List Note
applyBodyFilter str noteList =
    let
        f : Note -> Bool
        f note =
            String.contains (String.toLower str) (String.toLower note.body)

        select_ : Note -> Note
        select_ note =
            { note | selected = f note }
    in
    List.map select_ noteList


applyTagFilter : String -> List Note -> List Note
applyTagFilter str noteList =
    let
        f : Note -> Bool
        f note =
            List.member (String.toLower str) note.tags

        select_ : Note -> Note
        select_ note =
            { note | selected = f note }
    in
    List.map select_ noteList


firstSelectedNote : List Note -> Maybe Note
firstSelectedNote noteList =
    List.filter (\note -> note.selected) noteList |> List.head
