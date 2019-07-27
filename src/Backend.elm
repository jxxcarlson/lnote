module Backend exposing (Model, app, userList)

import Dict exposing (Dict)
import Frontend
import Lamdera.Backend
import Lamdera.Types exposing (..)
import Maybe.Extra
import Msg exposing (..)
import Note exposing (Note)
import Set exposing (Set)
import TestData exposing (passwordDict, userDict)
import User exposing (PasswordDict, User, UserDict, UserInfo, Username)
import UserData


app =
    Lamdera.Backend.application
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }



--
-- MODEL
--


type alias Model =
    { passwordDict : PasswordDict
    , userDict : UserDict Note
    , clients : Set ClientId
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { passwordDict = TestData.passwordDict
      , userDict = TestData.userDict
      , clients = Set.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        -- Our sendToFrontend Cmd has completed
        SentToFrontendResult clientId result ->
            case result of
                Ok () ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RequestUsers ->
            ( model, sendToFrontend clientId (SendUserList (userList model.userDict)) )

        SendSignInInfo username password ->
            case User.validateUser model.passwordDict username password of
                True ->
                    ( model, sendToFrontend clientId <| SendValidatedUser (User.fromDict model.userDict username) )

                False ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        SendChangePasswordInfo username password newPassword ->
            case User.validateUser model.passwordDict username password of
                True ->
                    let
                        passwordUpdater =
                            Maybe.map (\ep -> User.encrypt newPassword)

                        newPasswordDict =
                            Dict.update username passwordUpdater model.passwordDict
                    in
                    ( { model | passwordDict = newPasswordDict }, sendToFrontend clientId <| SendMessage "Password changed" )

                False ->
                    ( model, sendToFrontend clientId <| SendMessage "Could not change password" )

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

        SendNoteToBackend maybeUserName note ->
            case maybeUserName of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | userDict = UserData.update user.username note model.userDict }, Cmd.none )

        CreateNote maybeUsername note ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    case UserData.create user.username note model.userDict of
                        Err str ->
                            ( model, Cmd.none )

                        Ok ( newNote, userDict ) ->
                            ( { model | userDict = userDict }
                            , sendToFrontend clientId (SendNoteToFrontend newNote)
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
                    let
                        -- NOTE: very bad code, use for now
                        userDict =
                            UserData.update user.username note model.userDict

                        userNotes =
                            Dict.get user.username userDict
                                |> Maybe.map .data
                                |> Maybe.withDefault []
                    in
                    ( { model | userDict = userDict }
                    , sendToFrontend clientId (SendNotesToFrontend userNotes)
                    )

        ClientJoin ->
            ( model, Cmd.none )


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.Backend.sendToFrontend 1000 clientId (\_ -> NoOpBackendMsg) msg



--
-- HELPERS
--


userList : UserDict Note -> List User
userList userDict =
    List.map (User.fromDict userDict) (Dict.keys userDict)
        |> Maybe.Extra.values
