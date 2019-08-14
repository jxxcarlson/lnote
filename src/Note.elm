module Note exposing
    ( Note
    , SortDirection(..)
    , applyBodyFilter
    , applySubjectFilter
    , applyTagFilter
    , bigDateFilter
    , conjunctiveSearch
    , filterBySubject
    , filterByTag
    , filterText
    , firstSelectedNote
    , frequencies
    , fuzzyListMember
    , fuzzyMatchWordList
    , kDaysAgo
    , listToYaml
    , make
    , matchWordList
    , remove
    , replace
    , select
    , selectAll
    , selectSublist
    , sortAlphabetically
    , sortByTimeModified
    , stringContainsWords
    , tagsFromString
    , toYaml
    , wordCount
    )

import FrequencyDict exposing (FrequencyDict)
import List.Extra
import Random
import Time exposing (Posix, toHour, toMinute, toSecond, utc)


type alias Note =
    { id : Int
    , subject : String
    , tags : List String
    , body : String
    , timeCreated : Posix
    , timeModified : Posix
    , selected : Bool
    }


wordCount : Note -> Int
wordCount note =
    note.body |> String.words |> List.length


listToYaml : List Note -> String
listToYaml noteList =
    "---\n"
        ++ List.foldl (\note acc -> toYaml note ++ acc) "" noteList


toYaml : Note -> String
toYaml note =
    "- note\n"
        ++ "   - id: "
        ++ ("   " ++ String.fromInt note.id)
        ++ "\n"
        ++ "   - subject: "
        ++ note.subject
        ++ "\n"
        ++ "   - tags: ["
        ++ String.join ", " note.tags
        ++ "]\n"
        ++ "   - timeCreated: "
        ++ toUtcString note.timeCreated
        ++ "\n"
        ++ "   - timeModified: "
        ++ toUtcString note.timeModified
        ++ "\n"
        ++ "   - selected: "
        ++ stringFromBool note.selected
        ++ "\n"
        ++ "   - body: |\n"
        ++ bodyToYaml note.body
        ++ "\n"


bodyToYaml : String -> String
bodyToYaml str =
    str
        |> String.lines
        |> List.map (\s -> "      " ++ s)
        |> String.join "\n"


stringFromBool : Bool -> String
stringFromBool bit =
    case bit of
        True ->
            "True"

        False ->
            "False"


toUtcString : Time.Posix -> String
toUtcString time =
    String.fromInt (toHour utc time)
        ++ ":"
        ++ String.fromInt (toMinute utc time)
        ++ ":"
        ++ String.fromInt (toSecond utc time)
        ++ " (UTC)"


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



--
-- FILTERING
--


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


filterBySubject : String -> List Note -> List Note
filterBySubject filterString notes =
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



--
-- TAGS
--


tagsFromString : String -> List String
tagsFromString str =
    str
        |> String.split ","
        |> List.map String.trim


frequencies : List Note -> FrequencyDict
frequencies noteList =
    FrequencyDict.make (List.map .tags noteList |> List.concat)



-- SELECTING --


selectSublist : List Int -> List Note -> List Note
selectSublist intList noteList =
    let
        selectNote : Int -> Note -> Note
        selectNote k note =
            if List.member k intList then
                { note | selected = True }

            else
                { note | selected = False }
    in
    List.indexedMap selectNote noteList


selectAll : List Note -> List Note
selectAll noteList =
    noteList
        |> List.map (\note -> { note | selected = True })


select : Note -> Note
select note =
    { note | selected = True }


stringContainsWords : List String -> String -> Bool
stringContainsWords wordList str =
    let
        wordList_ =
            List.map String.toLower wordList

        str_ =
            String.toLower str
    in
    List.foldl (\w acc -> String.contains w str_ && acc) True wordList_


conjunctiveSearch : String -> String -> Bool
conjunctiveSearch key target =
    stringContainsWords (String.words key) target


matchWordList : List String -> List String -> Bool
matchWordList keyList targetList =
    List.foldl (\k acc -> List.member k targetList && acc) True keyList


fuzzyMatchWordList : List String -> List String -> Bool
fuzzyMatchWordList keyList targetList =
    List.foldl (\k acc -> fuzzyListMember k targetList && acc) True keyList


fuzzyListMember : String -> List String -> Bool
fuzzyListMember str strList =
    List.foldl (\w acc -> String.contains str w || acc) False strList


applySubjectFilter : String -> List Note -> List Note
applySubjectFilter str noteList =
    let
        f : Note -> Bool
        f note =
            conjunctiveSearch str note.subject

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
            conjunctiveSearch str note.body

        select_ : Note -> Note
        select_ note =
            { note | selected = f note }
    in
    List.map select_ noteList


applyTagFilter : String -> List Note -> List Note
applyTagFilter str noteList =
    let
        keys =
            str |> String.toLower |> String.words

        f : Note -> Bool
        f note =
            --List.member (String.toLower str) note.tags
            fuzzyMatchWordList keys note.tags

        select_ : Note -> Note
        select_ note =
            { note | selected = f note }
    in
    List.map select_ noteList


firstSelectedNote : List Note -> Maybe Note
firstSelectedNote noteList =
    List.filter (\note -> note.selected) noteList |> List.head



--
-- SORTING
--


type SortDirection
    = SortIncreasing
    | SortDecreasing
    | Unsorted


sortByTimeModified : SortDirection -> List Note -> List Note
sortByTimeModified sortDirection noteList =
    case sortDirection of
        SortIncreasing ->
            List.sortBy (\note -> note.timeCreated |> Time.posixToMillis) noteList

        SortDecreasing ->
            List.sortBy (\note -> note.timeCreated |> Time.posixToMillis |> (\x -> -x)) noteList

        Unsorted ->
            noteList


sortAlphabetically sortDirection noteList =
    case sortDirection of
        SortIncreasing ->
            List.sortBy (\note -> note.subject) noteList

        SortDecreasing ->
            List.sortBy (\note -> note.subject) noteList |> List.reverse

        Unsorted ->
            noteList
