module Backend exposing (app, userList)

import Dict exposing (Dict)
import FrequencyDict exposing (FrequencyDict)
import Frontend
import Http
import Lamdera exposing (ClientId, SessionId)
import Maybe.Extra
import Note exposing (Note)
import Random
import Set exposing (Set)
import TestData exposing (passwordDict, userDict, userInfo1)
import Types exposing (..)
import UUID
import User exposing (PasswordDict, User, UserDict, UserInfo, Username)
import UserData


{-| After maxUUIDCount UUIDs are generated, get
a new seed from random.org
-}
maxUUIDCount : Int
maxUUIDCount =
    300


app =
    Lamdera.backend
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }



--
-- MODEL
--


init =
    ( { passwordDict = Dict.empty
      , userDict = Dict.empty
      , clients = Set.empty
      , randomSeed = Random.initialSeed 1727485
      , uuidCount = 0
      , randomAtmosphericInt = Nothing
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotAtomsphericRandomNumber result ->
            case result of
                Ok str ->
                    case String.toInt (String.trim str) of
                        Nothing ->
                            ( model, Cmd.none )

                        Just rn ->
                            let
                                newRandomSeed =
                                    Random.initialSeed rn
                            in
                            ( { model
                                | randomAtmosphericInt = Just rn
                                , randomSeed = newRandomSeed
                              }
                            , Cmd.none
                            )

                Err _ ->
                    ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RequestUsers ->
            ( model, sendToFrontend clientId (SendUserList (userList model.userDict)) )

        SendSignInInfo username password ->
            case User.validateUser model.passwordDict username password || True of
                True ->
                    let
                        ( fD, newUserDict ) =
                            updateFrequencyDictionary username model.userDict
                    in
                    ( { model | userDict = newUserDict }
                    , Cmd.batch
                        [ sendToFrontend clientId <| SendValidatedUser (User.fromDict model.userDict username)
                        , sendToFrontend clientId <| SendFrequencyDict fD
                        ]
                    )

                False ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        SendChangePasswordInfo username password newPassword ->
            case User.validateUser model.passwordDict username password || True of
                True ->
                    let
                        passwordUpdater =
                            Maybe.map (\ep -> User.encrypt newPassword)

                        newPasswordDict =
                            Dict.update username passwordUpdater model.passwordDict
                    in
                    ( { model | passwordDict = newPasswordDict }, sendToFrontend clientId <| SendMessage <| "Password changed" )

                False ->
                    ( model, sendToFrontend clientId <| SendMessage "Could not change password!!" )

        SendSignUpInfo username password email ->
            case User.add username password email ( model.passwordDict, model.userDict ) of
                Ok ( newPasswordDict, newUserDict ) ->
                    ( { model | userDict = newUserDict, passwordDict = newPasswordDict }
                    , sendToFrontend clientId <| SendValidatedUser (User.fromDict newUserDict username)
                    )

                Err str ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        RequestNotes maybeUser ->
            case maybeUser of
                Nothing ->
                    ( model, sendToFrontend clientId (SendNotesToFrontend []) )

                Just user ->
                    let
                        userNotes =
                            Dict.get user.username model.userDict
                                |> Maybe.map .data
                                |> Maybe.withDefault []
                    in
                    ( model, sendToFrontend clientId (SendNotesToFrontend userNotes) )

        CreateNote maybeUsername note ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    let
                        ( newUUIDCount, getRandomNumberCmd ) =
                            case model.uuidCount < maxUUIDCount of
                                True ->
                                    ( model.uuidCount + 1, Cmd.none )

                                False ->
                                    ( 0, getRandomNumber )

                        ( newUUID, newSeed ) =
                            Random.step UUID.generator model.randomSeed

                        note_ =
                            { note | id = UUID.toString newUUID }
                    in
                    case UserData.create user.username note_ model.userDict of
                        Err str ->
                            ( model, Cmd.none )

                        Ok ( newNote, userDict ) ->
                            let
                                ( fD, newUserDict ) =
                                    updateFrequencyDictionary user.username userDict
                            in
                            ( { model
                                | userDict = newUserDict
                                , uuidCount = newUUIDCount
                                , randomSeed = newSeed
                              }
                            , Cmd.batch
                                [ sendToFrontend clientId (SendNoteToFrontend newNote)
                                , sendToFrontend clientId <| SendFrequencyDict fD
                                , getRandomNumberCmd
                                ]
                            )

        DeleteNote maybeUsername note ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | userDict = UserData.delete user.username note model.userDict }, Cmd.none )

        UpdateNote maybeUsername note ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    case UserData.update user.username note model.userDict of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok ( note_, userDict ) ->
                            ( { model | userDict = userDict }
                            , sendToFrontend clientId (SendNoteToFrontend note_)
                            )

        UpdateTags maybeUsername note ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    case UserData.update user.username note model.userDict of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok ( note_, userDict ) ->
                            let
                                ( fD, newUserDict ) =
                                    updateFrequencyDictionary user.username userDict
                            in
                            ( { model | userDict = newUserDict }
                            , Cmd.batch
                                [ sendToFrontend clientId (SendNoteToFrontend note_)
                                , sendToFrontend clientId <| SendFrequencyDict fD
                                ]
                            )

        ClientJoin ->
            ( model
            , sendToFrontend clientId <| SendMessage <| "Hi there!"
            )


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.sendToFrontend clientId msg



--
-- HELPERS
--


userList : UserDict Note -> List User
userList userDict =
    List.map (User.fromDict userDict) (Dict.keys userDict)
        |> Maybe.Extra.values


updateFrequencyDictionary : Username -> UserDict Note -> ( FrequencyDict, UserDict Note )
updateFrequencyDictionary username userDict =
    case Dict.get username userDict of
        Nothing ->
            ( Dict.empty, userDict )

        Just userInfo ->
            let
                fD =
                    Note.frequencies userInfo.data

                newUserInfo =
                    { userInfo | tagDict = fD }
            in
            ( fD, Dict.insert username newUserInfo userDict )


getRandomNumber : Cmd BackendMsg
getRandomNumber =
    Http.get
        { url = randomNumberUrl 9
        , expect = Http.expectString GotAtomsphericRandomNumber
        }


{-| maxDigits < 10
-}
randomNumberUrl : Int -> String
randomNumberUrl maxDigits =
    let
        maxNumber =
            10 ^ maxDigits

        prefix =
            "https://www.random.org/integers/?num=1&min=1&max="

        suffix =
            "&col=1&base=10&format=plain&rnd=new"
    in
    prefix ++ String.fromInt maxNumber ++ suffix
