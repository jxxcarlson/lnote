module Frontend exposing (Model, app)

--
-- import Main exposing (UserMode)
-- import Svg.Attributes exposing (k1)
-- exposing (..)
-- import Date exposing (Date)

import Array exposing (map)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import DateTime
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graph exposing (Option(..))
import Html exposing (Html, time)
import Lamdera.Frontend as Frontend
import Lamdera.Types exposing (..)
import Msg exposing (AppMode(..), BackendMsg(..), DeleteNoteSafety(..), FrontendMsg(..), ToBackend(..), ToFrontend(..), ValidationState(..))
import Note exposing (Note)
import Style
import Task
import TestData exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (User, Username)
import Utility


app =
    Frontend.application
        { init = \_ _ -> init
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Lamdera Alpha"
                , body = [ view model ]
                }
        }



--
-- TYPES
--
--
-- MODEL
--


type alias Model =
    { input : String
    , appMode : AppMode
    , message : String

    -- USER
    , currentUser : Maybe User
    , username : String
    , password : String
    , newPassword1 : String
    , newPassword2 : String
    , email : String
    , userList : List User

    -- NOTES
    , noteList : List Note
    , deleteNoteSafety : DeleteNoteSafety
    , noteCameBeforeString : String
    , noteCameAfterString : String
    , maybeCurrentNote : Maybe Note
    }



--
-- INIT
--


initialModel =
    { input = "App started"
    , message = "Please sign in"
    , appMode = UserValidation SignInState

    -- ADMIN
    , userStats = Dict.empty

    -- USER
    , currentUser = Nothing
    , username = ""
    , password = ""
    , newPassword1 = ""
    , newPassword2 = ""
    , email = ""
    , userList = []

    -- NOTES
    , noteList = []
    , deleteNoteSafety = DeleteNoteSafetyOn
    , noteCameBeforeString = ""
    , noteCameAfterString = ""
    }


init : ( Model, Cmd FrontendMsg )
init =
    ( initialModel, sendToBackend timeoutInMs SentToBackendResult ClientJoin )


timeoutInMs =
    5 * 1000


sendToBackend : Milliseconds -> (Result WsError () -> FrontendMsg) -> ToBackend -> Cmd FrontendMsg
sendToBackend =
    Frontend.sendToBackend


subscriptions model =
    Time.every 1000 TimeChange



--
-- UPDATE
--


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        SendMessage str ->
            ( { model | message = str }, Cmd.none )

        SendNotesToFrontend newNoteList ->
            ( { model | notes = newNoteList }, Cmd.none )

        SendUserList userList ->
            ( { model | userList = userList }, Cmd.none )

        SendValidatedUser currentUser ->
            case currentUser of
                Nothing ->
                    ( { model | currentUser = Nothing, message = "Incorrect password/username" }, Cmd.none )

                Just user ->
                    ( { model | currentUser = Just user, appMode = UserMode }
                    , sendToBackend timeoutInMs SentToBackendResult (RequestNotes (Just user))
                    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        -- ADMIN
        SendUsers ->
            case currentUserIsAdmin model of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( model
                    , sendToBackend timeoutInMs SentToBackendResult RequestUsers
                    )

        -- BACKEND
        SentToBackendResult result ->
            ( model, Cmd.none )

        -- URL (NOT USED)
        ChangeUrl url ->
            ( model, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Cmd.none )

                External url ->
                    ( model, Cmd.none )

        -- UI
        SetAppMode mode ->
            let
                cmd =
                    case mode of
                        Admin ->
                            sendToBackend timeoutInMs SentToBackendResult RequestUsers

                        _ ->
                            Cmd.none

                message =
                    case mode of
                        UserValidation SignInState ->
                            "Please sign in"

                        UserValidation SignUpState ->
                            "Please sign up"

                        _ ->
                            ""
            in
            ( { model
                | appMode = mode
                , message = message
              }
            , cmd
            )

        SetDeleteNoteSafety deleteNoteSafetyState ->
            ( { model | deleteNoteSafety = deleteNoteSafetyState }, Cmd.none )

        -- USER
        GotUserName str ->
            ( { model | username = str }, Cmd.none )

        GotPassword str ->
            ( { model | password = str }, Cmd.none )

        GotNewPassword1 str ->
            ( { model | newPassword1 = str }, Cmd.none )

        GotNewPassword2 str ->
            ( { model | newPassword2 = str }, Cmd.none )

        ChangePassword ->
            case User.validateChangePassword model.newPassword1 model.newPassword2 of
                [] ->
                    case model.currentUser of
                        Nothing ->
                            ( { model | message = "No user signed in" }, Cmd.none )

                        Just user ->
                            ( { model | message = "OK" }
                            , sendToBackend timeoutInMs SentToBackendResult (SendChangePasswordInfo user.username model.password model.newPassword1)
                            )

                errorList ->
                    ( { model | message = String.join ", " errorList }, Cmd.none )

        GotEmail str ->
            ( { model | email = str }, Cmd.none )

        SignIn ->
            ( initialModel, sendToBackend timeoutInMs SentToBackendResult (SendSignInInfo model.username model.password) )

        SignUp ->
            let
                signUpErrors =
                    User.validateSignUpInfo model.username model.password model.email
            in
            case List.length signUpErrors > 0 of
                True ->
                    ( { model | message = String.join ", " signUpErrors }, Cmd.none )

                False ->
                    ( initialModel, sendToBackend timeoutInMs SentToBackendResult (SendSignUpInfo model.username model.password model.email) )

        SignOut ->
            ( initialModel, Cmd.none )

        -- NOtE
        GotNoteFilter str ->
            ( { model | noteFilterString = str }, Cmd.none )

        GotNoteDateAfterFilter str ->
            ( { model | noteCameAfterString = str }, Cmd.none )

        GotNoteDateBeforeFilter str ->
            ( { model | noteCameBeforeString = str }, Cmd.none )

        MakeNewNote ->
            case newNote model of
                Nothing ->
                    ( model, Cmd.none )

                Just n ->
                    ( { model | maybeCurrentNote = Just n, notes = n :: model.notes }
                    , sendToBackend timeoutInMs SentToBackendResult (CreateNote model.currentUser n)
                    )

        DeleteCurrentNote ->
            case model.maybeCurrentNote of
                Nothing ->
                    ( { model | deleteNoteSafety = DeleteNoteSafetyOn }, Cmd.none )

                Just note ->
                    ( { model | maybeCurrentNote = Nothing, deleteNoteSafety = DeleteNoteSafetyOn }
                    , sendToBackend timeoutInMs SentToBackendResult (DeleteNote model.currentUser note)
                    )

        GotNewNoteName str ->
            ( { model | newNoteName = str }, Cmd.none )

        GotChangedNoteName str ->
            ( { model | changedNoteName = str }, Cmd.none )



--
-- VIEW
--


view : Model -> Html FrontendMsg
view model =
    Element.layout [] (mainView model)


mainView : Model -> Element FrontendMsg
mainView model =
    column []
        [ header model
        , case model.appMode of
            UserValidation _ ->
                userValidationView model

            UserMode ->
                masterView model

            Admin ->
                adminView model
        ]



--
-- USER VIEW
--


userValidationView : Model -> Element FrontendMsg
userValidationView model =
    case model.currentUser of
        Nothing ->
            noUserView model

        Just user ->
            signedInUserView model user


noUserView : Model -> Element FrontendMsg
noUserView model =
    column Style.mainColumnX
        [ el [ Font.size 18, Font.bold, paddingXY 0 12 ] (text "Welcome!")
        , inputUserName model
        , inputPassword model
        , showIf (model.appMode == UserValidation SignUpState) (inputEmail model)
        , showIf (model.appMode == UserValidation SignUpState) (el [ Font.size 12 ] (text "A real email address is only needed for password recovery in real production."))
        , row [ spacing 12, paddingXY 0 12 ]
            [ showIf (model.appMode == UserValidation SignInState) (signInButton model)
            , row [ spacing 12 ]
                [ signUpButton model
                , showIf (model.appMode == UserValidation SignUpState) (cancelSignUpButton model)
                ]
            ]
        , el [ Font.size 12 ] (text model.message)
        ]


signedInUserView : Model -> User -> Element FrontendMsg
signedInUserView model user =
    column Style.mainColumnX
        [ el [] (text <| "Signed in as " ++ user.username)
        , signOutButton model
        , showIf (model.appMode == UserValidation ChangePasswordState) (passwordPanel model)
        , row [ spacing 12 ]
            [ changePasswordButton model
            , showIf (model.appMode == UserValidation ChangePasswordState) (cancelChangePasswordButton model)
            ]
        , adminStatus model
        ]


passwordPanel model =
    column [ spacing 12, paddingXY 0 18 ]
        [ inputCurrentPassword model
        , inputNewPassword1 model
        , inputNewPassword2 model
        , el [ Font.size 12 ] (text model.message)
        ]


inputCurrentPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "Old password: ")
        }


inputNewPassword1 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword1
        , show = False
        , text = model.newPassword1
        , placeholder = Nothing

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "New password: ")
        }


inputNewPassword2 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword2
        , text = model.newPassword2
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "Password again: ")
        }


changePasswordButton : Model -> Element FrontendMsg
changePasswordButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation ChangePasswordState ->
                    Just ChangePassword

                _ ->
                    Just <| SetAppMode (UserValidation ChangePasswordState)
        , label = Element.text "Change password"
        }


adminStatus : Model -> Element FrontendMsg
adminStatus model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    el [ Font.size 12 ] (text "Admin")


inputUserName model =
    Input.text (Style.inputStyle 200)
        { onChange = GotUserName
        , text = model.username
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Username")
        }


inputEmail model =
    Input.text (Style.inputStyle 200)
        { onChange = GotEmail
        , text = model.email
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Email")
        }


inputPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Password")
        }


signInButton : Model -> Element FrontendMsg
signInButton model =
    Input.button Style.headerButton
        { onPress = Just SignIn
        , label = Element.text "Sign in"
        }


signUpButton : Model -> Element FrontendMsg
signUpButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation SignUpState ->
                    Just SignUp

                _ ->
                    Just (SetAppMode (UserValidation SignUpState))
        , label = Element.text "Sign Up"
        }


cancelSignUpButton : Model -> Element FrontendMsg
cancelSignUpButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation SignUpState ->
                    Just (SetAppMode (UserValidation SignInState))

                _ ->
                    Just NoOpFrontendMsg
        , label = Element.text "Cancel"
        }


cancelChangePasswordButton : Model -> Element FrontendMsg
cancelChangePasswordButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation ChangePasswordState ->
                    Just (SetAppMode (UserValidation SignInState))

                _ ->
                    Just NoOpFrontendMsg
        , label = Element.text "Cancel"
        }


signOutButton : Model -> Element FrontendMsg
signOutButton model =
    Input.button Style.headerButton
        { onPress = Just SignOut
        , label = Element.text "Sign out"
        }



--
-- HEADER
--


header : Model -> Element FrontendMsg
header model =
    row
        [ width fill
        , paddingXY 40 8
        , Background.color Style.charcoal
        , spacing 12
        ]
        [ showIf (currentUserIsAdmin model) (adminModeButton model)
        , userValidationModeButton model
        , el [ centerX, Font.size 18, Font.color Style.white ] (text <| "Time Note" ++ currentUserName model)
        ]


currentUserIsAdmin : Model -> Bool
currentUserIsAdmin model =
    case model.currentUser of
        Nothing ->
            False

        Just user ->
            user.admin


currentUserName : Model -> String
currentUserName model =
    case model.currentUser of
        Nothing ->
            ""

        Just user ->
            " for " ++ user.username


userValidationModeButton : Model -> Element FrontendMsg
userValidationModeButton model =
    Input.button ((Style.select <| model.appMode == UserValidation SignInState) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode (UserValidation SignInState))
        , label = Element.text "User"
        }


adminModeButton : Model -> Element FrontendMsg
adminModeButton model =
    Input.button ((Style.select <| model.appMode == Admin) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode Admin)
        , label = Element.text "Admin"
        }


notegingModeButton : Model -> Element FrontendMsg
notegingModeButton model =
    Input.button ((Style.select <| model.appMode == UserMode) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode UserMode)
        , label = Element.text "Notes"
        }



--
-- LOG ROW
--


masterView : Model -> Element FrontendMsg
masterView model =
    column Style.mainColumnX
        [ filterPanel model
        , row []
            [ noteListPanel model
            , eventListDisplay model
            , eventPanel model
            ]
        , column [ spacing 12 ]
            [ row [ spacing 12 ] [ newNoteButton, inputNewNoteName model ]
            , row [ spacing 12 ]
                [ el [] (text "controls")
                ]
            ]
        ]


deleteNoteControls model =
    row [ spacing 12 ]
        [ deleteNoteButton model
        , showIf (model.deleteNoteSafety == DeleteNoteSafetyOff) cancelDeleteNoteButton
        ]


noteControls model =
    row [ spacing 12 ]
        [ el [] (text "noteControls")
        ]


newNoteButton : Element FrontendMsg
newNoteButton =
    Input.button Style.button
        { onPress = Just MakeNewNote
        , label = Element.text "New note"
        }


cancelDeleteNoteButton : Element FrontendMsg
cancelDeleteNoteButton =
    Input.button Style.button
        { onPress = Just <| SetDeleteNoteSafety DeleteNoteSafetyOn
        , label = Element.text "Cancel"
        }


deleteNoteButton : Model -> Element FrontendMsg
deleteNoteButton model =
    case model.deleteNoteSafety of
        DeleteNoteSafetyOn ->
            Input.button Style.button
                { onPress = Just <| SetDeleteNoteSafety DeleteNoteSafetyOff
                , label = Element.text "Delete note"
                }

        DeleteNoteSafetyOff ->
            Input.button Style.dangerousButton
                { onPress = Just DeleteCurrentNote
                , label = Element.text "Delete forever?"
                }


inputNewNoteName model =
    Input.text (Style.inputStyle 200)
        { onChange = GotNewNoteName
        , text = model.newNoteName
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }



--
--
-- VIEWLOGS
--


viewNote : Model -> Element FrontendMsg
viewNote model =
    case model.maybeCurrentNote of
        Nothing ->
            column [ spacing 12, padding 20, height (px 500) ]
                [ el [ Font.size 16, Font.bold ] (text "No events available")
                ]

        Just currentNote ->
            let
                today =
                    model.currentTime

                events2 =
                    Note.bigDateFilter today model.noteCameBeforeString model.noteCameAfterString currentNote.data

                -- events : List Note
                -- events =
                --     Note.groupingFilter model.filterState events2
                events =
                    case model.filterState of
                        NoGrouping ->
                            events2

                        GroupByDay ->
                            Note.eventsByDay events2

                eventSum_ =
                    Note.eventSum events

                nNotes =
                    List.length events |> toFloat

                average =
                    TypedTime.multiply (1.0 / nNotes) eventSum_
            in
            column [ spacing 12, padding 20, height (px 430) ]
                [ el [ Font.size 16, Font.bold ] (text (Maybe.map .name model.maybeCurrentNote |> Maybe.withDefault "XXX"))
                , indexedTable [ spacing 4, Font.size 12, height (px 400), scrollbarY ]
                    { data = events
                    , columns =
                        [ { header = el [ Font.bold ] (text <| idLabel model)
                          , width = px (indexWidth model.appMode)
                          , view = indexButton model
                          }
                        , { header = el [ Font.bold ] (text "Date")
                          , width = px 80

                          --, view = \k event -> el [ Font.size 12 ] (text <| dateStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          , view = \k event -> el [ Font.size 12 ] (text <| DateTime.humanDateStringFromPosix <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Time")
                          , width = px 80
                          , view = \k event -> el [ Font.size 12 ] (text <| DateTime.naiveTimeStringFromPosix <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Duration")
                          , width = px 40
                          , view = \k event -> el [ Font.size 12 ] (text <| TypedTime.timeAsStringWithUnit Minutes event.duration)
                          }
                        ]
                    }
                , row [ spacing 24, alignBottom, alignRight ]
                    [ el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Count: " ++ String.fromInt (List.length events))
                    , el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Average: " ++ TypedTime.timeAsStringWithUnit Minutes average)
                    , el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Total: " ++ TypedTime.timeAsStringWithUnit Minutes eventSum_)
                    ]
                ]


idLabel : Model -> String
idLabel model =
    case model.appMode of
        Editing ->
            "id"

        _ ->
            "idx"



--
-- VIEWEVENTS
--


eventPanel : Model -> Element FrontendMsg
eventPanel model =
    case model.maybeCurrentNote of
        Nothing ->
            Element.none

        Just currentNote ->
            let
                events2 =
                    Note.bigDateFilter model.currentTime model.noteCameBeforeString model.noteCameAfterString currentNote.data

                events =
                    case model.filterState of
                        NoGrouping ->
                            events2

                        GroupByDay ->
                            Note.eventsByDay events2
            in
            column [ Font.size 12, spacing 36, moveRight 40, width (px 400) ]
                [ row [ moveLeft 40 ] [ graph model events ]
                , row [ spacing 16 ]
                    [ row [ spacing 8 ] [ setMinutesButton model, setHoursButton model ]
                    , row [ spacing 8 ] [ el [ Font.bold, Font.size 14 ] (text "Group:"), noFilterButton model, filterByDayButton model ]
                    ]
                , newNotePanel 350 model
                ]


noteNotePanel model =
    case model.maybeCurrentNote of
        Nothing ->
            Element.none

        Just evt ->
            column [ width (px 300), height (px 450), padding 12, Border.width 1, spacing 36 ]
                [ el [ Font.bold ] (text <| "Edit event " ++ String.fromInt evt.id)
                , column [ spacing 12 ]
                    [ inputChangeNoteDuration model
                    , changeDurationButton model
                    ]
                , row [ spacing 12 ]
                    [ deleteNoteButton model
                    , showIf (model.deleteNoteSafety == DeleteNoteSafetyOff) cancelDeleteNoteButton
                    ]
                ]


graph model events_ =
    let
        events__ =
            List.reverse events_
    in
    Graph.barChart (gA model) (prepareData (getScaleFactor model) events__) |> Element.html



--
--
--


newNotePanel : Int -> Model -> Element FrontendMsg
newNotePanel w model =
    column [ spacing 24, width (px w) ]
        [ row [ Border.width 1, padding 12, spacing 12, width (px 300) ] [ submitNoteButton, inputNoteDuration model ]
        , largeElapsedTimePanel model
        ]


prepareData : Float -> List Note -> List Float
prepareData scaleFactor_ eventList =
    List.map (floatValueOfNote scaleFactor_) eventList


floatValueOfNote : Float -> Note -> Float
floatValueOfNote scaleFactor_ event =
    event |> .duration |> convertToSeconds |> (\x -> x / scaleFactor_)


submitNoteButton : Element FrontendMsg
submitNoteButton =
    Input.button Style.button
        { onPress = Just MakeNote
        , label = Element.text "New: minutes or hh:mm"
        }



-- VIEW HELPERS
--


showIf : Bool -> Element FrontendMsg -> Element FrontendMsg
showIf bit element =
    if bit then
        element

    else
        Element.none


showOne : Bool -> String -> String -> String
showOne bit str1 str2 =
    case bit of
        True ->
            str1

        False ->
            str2


newNote : Model -> Maybe Note
newNote model =
    case ( model.currentUser, String.length model.newNoteName > 0 ) of
        ( Just user, True ) ->
            let
                now =
                    case model.currentTime of
                        Nothing ->
                            Time.millisToPosix 0

                        Just t ->
                            Time.millisToPosix t
            in
            Just <|
                { id = -1
                , counter = 0
                , name = model.newNoteName
                , body = ""
                , timeCreated = now
                , timeModfied = now
                , tags = []
                }

        _ ->
            Nothing



--
-- ADMIN VIEW
--


adminView : Model -> Element FrontendMsg
adminView model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    adminView_ model user


adminView_ : Model -> User -> Element FrontendMsg
adminView_ model user =
    column Style.mainColumnX
        [ el [ Font.size 14 ] (text <| "Admin: " ++ user.username)
        , indexedTable
            [ spacing 4, Font.size 12, paddingXY 0 12, height (px 300), scrollbarY ]
            { data = model.userList
            , columns =
                [ { header = el [ Font.bold ] (text "k")
                  , width = px 40
                  , view = \k usr -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                  }
                , { header = el [ Font.bold ] (text "Username")
                  , width = px 80
                  , view = \k usr -> el [ Font.size 12 ] (text usr.username)
                  }
                , { header = el [ Font.bold ] (text "Email")
                  , width = px 200
                  , view = \k usr -> el [ Font.size 12 ] (text usr.email)
                  }
                , { header = el [ Font.bold ] (text "Notes")
                  , width = px 80
                  , view = \k usr -> el [ Font.size 12 ] (text <| displayNumberOfNotes usr.username model.userStats)
                  }
                , { header = el [ Font.bold ] (text "Notes")
                  , width = px 80
                  , view = \k usr -> el [ Font.size 12 ] (text <| displayNumberOfNotes usr.username model.userStats)
                  }
                ]
            }
        , cleanDataButton model
        ]


eventListDisplay : Model -> Element FrontendMsg
eventListDisplay model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewNote model
        ]


filterPanel model =
    row [ spacing 8 ]
        [ el [ Font.bold ] (text "Filter:")
        , inputNoteNameFilter model
        , row [ spacing 8 ]
            [ el [ Font.bold, Font.size 14 ] (text "After")
            , displayShiftedDate model.noteCameAfterString model.currentTime
            ]
        , inputNoteCameAfterFilter model
        , row [ spacing 8 ]
            [ el [ Font.bold, Font.size 14 ] (text "Before")
            , displayShiftedDate model.noteCameBeforeString model.currentTime
            ]
        , inputNoteCameBeforeFilter model

        --, row [ alignRight, moveRight 36, spacing 12 ] [ editModeButton sharedState model, noteModeButton model ]
        ]


displayShiftedDate : String -> Posix -> Element FrontendMsg
displayShiftedDate kDaysAgoString today =
    case String.toInt kDaysAgoString of
        Nothing ->
            el [ width (px 75), Font.bold, Font.size 14 ] (text "(days)")

        Just k ->
            let
                shiftedDate =
                    Note.kDaysAgo k today
            in
            el [ width (px 75), Font.bold, Font.size 14 ] (text <| DateTime.humanDateStringFromPosix shiftedDate)


inputNoteNameFilter model =
    Input.text (Style.inputStyle 200)
        { onChange = GotNoteFilter
        , text = model.noteFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputNoteCameBeforeFilter model =
    Input.text (Style.inputStyle 50)
        { onChange = GotNoteDateBeforeFilter
        , text = model.noteCameBeforeString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputNoteCameAfterFilter model =
    Input.text (Style.inputStyle 50)
        { onChange = GotNoteDateAfterFilter
        , text = model.noteCameAfterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


noteListPanel : Model -> Element FrontendMsg
noteListPanel model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewNotes model
        ]


viewNotes : Model -> Element FrontendMsg
viewNotes model =
    let
        idx k note =
            String.fromInt note.id
    in
    column [ spacing 12, padding 20, height (px 400) ]
        [ el [ Font.size 16, Font.bold ] (text "Notes")
        , indexedTable
            [ spacing 4, Font.size 12, height (px 370), width (px 300), scrollbarY ]
            { data = Note.filter model.noteFilterString model.notes
            , columns =
                [ { header = el [ Font.bold ] (text "k")
                  , width = px 20
                  , view = \k note -> el [ Font.size 12 ] (text <| idx k note)
                  }
                , { header = el [ Font.bold ] (text "Name")
                  , width = px 170
                  , view = \k note -> el [ Font.size 12 ] (noteNameButton model.maybeCurrentNote note)
                  }
                ]
            }
        ]


noteNameButton : Maybe Note -> Note -> Element FrontendMsg
noteNameButton currentNote note =
    Input.button (Style.titleButton (currentNote == Just note))
        { onPress = Just (GetNotes note.id)
        , label = Element.text note.name
        }


setCurrentNoteButton : Model -> Note -> Int -> Element FrontendMsg
setCurrentNoteButton model event index =
    Input.button (Style.titleButton (Just event == model.maybeCurrentNote))
        { onPress = Just (SetCurrentNote event)
        , label = el [ Font.bold ] (Element.text <| String.fromInt event.id)
        }



--
-- UPDATE HELPERS
--


type alias UpdateNoteRecord =
    { currentNote : Note
    , noteList : List Note
    , cmd : Cmd FrontendMsg
    }


addNoteUsingString : Maybe User -> String -> Posix -> Note -> List Note -> UpdateNoteRecord
addNoteUsingString maybeUser eventDurationString currentTime note noteList =
    case TypedTime.decodeHM eventDurationString of
        Nothing ->
            { currentNote = note, noteList = noteList, cmd = Cmd.none }

        Just duration ->
            addNote maybeUser (TypedTime.convertFromSecondsWithUnit Seconds duration) currentTime note noteList


addNote : Maybe User -> TypedTime -> Posix -> Note -> List Note -> UpdateNoteRecord
addNote maybeUser duration currentTime note noteList =
    let
        newNote_ =
            Note.insertNote "" duration currentTime note

        newNotes =
            Note.replaceNote newNote_ noteList

        cmd =
            sendToBackend timeoutInMs SentToBackendResult (SendNoteToBackend maybeUser newNote_)
    in
    { currentNote = newNote_, noteList = newNotes, cmd = cmd }


changeNoteUsingString : Maybe User -> String -> String -> Note -> Note -> List Note -> UpdateNoteRecord
changeNoteUsingString maybeUser note eventDurationString event note noteList =
    case TypedTime.decodeHM eventDurationString of
        Just duration ->
            changeNote maybeUser note (TypedTime.convertFromSecondsWithUnit Seconds duration) event note noteList

        Nothing ->
            { currentNote = note, noteList = noteList, cmd = Cmd.none }


changeNote : Maybe User -> String -> TypedTime -> Note -> Note -> List Note -> UpdateNoteRecord
changeNote maybeUser note duration event note noteList =
    let
        newNote_ =
            Note.updateNote "" duration event note

        newNotes =
            Note.replaceNote newNote_ noteList

        cmd =
            sendToBackend timeoutInMs SentToBackendResult (SendNoteToBackend maybeUser newNote_)
    in
    { currentNote = newNote_, noteList = newNotes, cmd = cmd }



--
-- END
--
