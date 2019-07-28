module FrequencyDict exposing (FrequencyDict, asString, list, make)

import Dict exposing (Dict)


type alias FrequencyDict =
    Dict String Int


{-|

> d = makeFrequencyDictionary ["a", "b", "a", "a", "b", "c"]
> Dict.fromList [("a",3),("b",2),("c",1)]

-}
make : List String -> FrequencyDict
make noteList =
    let
        addItem str dict =
            case Dict.get str dict of
                Nothing ->
                    Dict.insert str 1 dict

                Just k ->
                    Dict.insert str (k + 1) dict
    in
    List.foldl (\str dict -> addItem str dict) Dict.empty noteList


list : FrequencyDict -> List ( String, Int )
list dict =
    dict |> Dict.toList |> List.sortBy (\( k, v ) -> -v)


asString : Int -> FrequencyDict -> String
asString k fD =
    let
        itemAsString =
            \( str, i ) -> str ++ ": " ++ String.fromInt i
    in
    list fD
        |> List.take k
        |> List.map itemAsString
        |> String.join ", "
