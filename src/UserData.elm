module UserData exposing (create, delete, update)

import Dict exposing (Dict)
import Note exposing (Note)
import TypedTime exposing (TypedTime)
import User exposing (UserDict, UserInfo, Username)


create : Username -> Note -> UserDict Note -> UserDict Note
create username note userDict =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newNote =
                    { note | id = userInfo.counter + 1 }

                updater : Maybe (UserInfo Note) -> Maybe (UserInfo Note)
                updater =
                    Maybe.map (\uInfo -> { uInfo | counter = uInfo.counter + 1, data = newNote :: uInfo.data })
            in
            Dict.update username updater userDict


update : Username -> Note -> UserDict Note -> UserDict Note
update username note userDict =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newData =
                    Note.replace note userInfo.data

                newUserInfo =
                    { userInfo | data = newData }
            in
            Dict.update username (\x -> Just newUserInfo) userDict


delete : Username -> Note -> UserDict Note -> UserDict Note
delete username note userDict =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newData =
                    List.filter (\n -> n.id /= note.id) userInfo.data

                newUserInfo =
                    { userInfo | data = newData }
            in
            Dict.update username (\x -> Just newUserInfo) userDict
