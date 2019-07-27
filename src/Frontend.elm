module Frontend exposing (Model, app)

--
-- import Main exposing (UserNotes)
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
import Element.Keyed as Keyed
import Graph exposing (Option(..))
import Html exposing (Html, time)
import Html.Attributes as HA
import Lamdera.Frontend as Frontend
import Lamdera.Types exposing (..)
import Markdown
import Msg exposing (AppMode(..), BackendMsg(..), DeleteNoteSafety(..), FrontendMsg(..), NotesMode(..), ToBackend(..), ToFrontend(..), ValidationState(..))
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
                { title = "Lamdera Notes"
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
    , counter : Int
    , currentTime : Posix

    -- USER
    , currentUser : Maybe User
    , username : String
    , password : String
    , newPassword1 : String
    , newPassword2 : String
    , email : String
    , userList : List User

    -- NOTES
    , notes : List Note
    , maybeCurrentNote : Maybe Note
    , newSubject : String
    , changedSubject : String
    , noteBody : String
    , noteFilterString : String
    , textFilterString : String
    , noteCameBeforeString : String
    , noteCameAfterString : String
    , deleteNoteSafety : DeleteNoteSafety
    }



--
-- INIT
--


initialModel =
    { input = "App started"
    , message = "Please sign in"
    , appMode = UserValidation SignInState
    , currentTime = Time.millisToPosix 0
    , counter = 0

    -- ADMIN
    , -- USER
      currentUser = Nothing
    , username = ""
    , password = ""
    , newPassword1 = ""
    , newPassword2 = ""
    , email = ""
    , userList = []

    -- NOTES
    , notes = []
    , maybeCurrentNote = Nothing
    , noteBody = ""
    , newSubject = ""
    , changedSubject = ""
    , noteFilterString = ""
    , textFilterString = ""
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

        SendNoteToFrontend note ->
            ( { model
                | notes = Note.replace note model.notes
                , maybeCurrentNote = Just note
              }
            , Cmd.none
            )

        SendUserList userList ->
            ( { model | userList = userList }, Cmd.none )

        SendValidatedUser currentUser ->
            case currentUser of
                Nothing ->
                    ( { model | currentUser = Nothing, message = "Incorrect password/username" }, Cmd.none )

                Just user ->
                    ( { model | currentUser = Just user, appMode = UserNotes BrowsingNotes }
                    , sendToBackend timeoutInMs SentToBackendResult (RequestNotes (Just user))
                    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        TimeChange t ->
            ( { model | currentTime = t }, Cmd.none )

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
        SetCurrentNote note ->
            ( { model
                | maybeCurrentNote = Just note
                , changedSubject = note.subject
                , noteBody = note.body
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        GotNoteFilter str ->
            -- ###
            ( { model | noteFilterString = str }, Cmd.none )

        GotTextFilter str ->
            ( { model | textFilterString = str }, Cmd.none )

        GotNoteDateAfterFilter str ->
            ( { model | noteCameAfterString = str }, Cmd.none )

        GotNoteDateBeforeFilter str ->
            ( { model | noteCameBeforeString = str }, Cmd.none )

        MakeNewNote ->
            let
                n =
                    Note.make -1 "New Note" "XXX" model.currentTime
            in
            ( { model
                | appMode = UserNotes CreatingNote
                , maybeCurrentNote = Just n
                , noteBody = n.body
                , newSubject = n.subject
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        FECreateNote ->
            case newNote model of
                Nothing ->
                    ( model, Cmd.none )

                Just n ->
                    ( { model
                        | maybeCurrentNote = Just n
                        , changedSubject = n.subject
                        , noteBody = n.body
                        , appMode = UserNotes EditingNote
                        , counter = model.counter + 1
                      }
                    , sendToBackend timeoutInMs SentToBackendResult (CreateNote model.currentUser n)
                    )

        DoUpdateNote ->
            case model.maybeCurrentNote of
                Nothing ->
                    ( model, Cmd.none )

                Just note ->
                    let
                        updatedNote =
                            { note | subject = model.changedSubject, body = model.noteBody }
                    in
                    ( { model
                        | maybeCurrentNote = Just updatedNote
                        , changedSubject = updatedNote.subject
                        , newSubject = updatedNote.subject
                        , noteBody = updatedNote.body

                        -- , notes = Note.replace updatedNote model.notes
                      }
                    , sendToBackend timeoutInMs SentToBackendResult (UpdateNote model.currentUser updatedNote)
                    )

        DeleteCurrentNote ->
            case model.maybeCurrentNote of
                Nothing ->
                    ( { model | deleteNoteSafety = DeleteNoteSafetyOn }, Cmd.none )

                Just note ->
                    ( { model
                        | maybeCurrentNote = Nothing
                        , deleteNoteSafety = DeleteNoteSafetyOn
                        , notes = Note.remove note model.notes
                      }
                    , sendToBackend timeoutInMs SentToBackendResult (DeleteNote model.currentUser note)
                    )

        GotNewNoteName str ->
            ( { model | newSubject = str }, Cmd.none )

        GotNoteBody str ->
            let
                updatedNote_ =
                    case model.maybeCurrentNote of
                        Just note ->
                            { note | body = str }

                        Nothing ->
                            Note.make -1 "???" str model.currentTime
            in
            ( { model | noteBody = str, maybeCurrentNote = Just updatedNote_ }, Cmd.none )

        GotChangedSubject str ->
            ( { model | changedSubject = str }, Cmd.none )



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

            UserNotes _ ->
                masterView model

            Admin ->
                adminView model
        , showIf (model.currentUser /= Nothing)
            (row [ width fill, spacing 18, Background.color Style.charcoal, paddingXY 8 8 ]
                [ el [ Font.bold, Font.color Style.white ] (text "Note:")
                , setNoteModeButton BrowsingNotes "Browse" model
                , setNoteModeButton EditingNote "Edit" model
                , makeNewNoteButton model
                , row [ paddingXY 24 0 ] [ showIf (model.maybeCurrentNote /= Nothing) (deleteNoteButton model) ]
                ]
            )
        , showIf (model.currentUser == Nothing)
            (row [ width fill, spacing 18, Background.color Style.charcoal, paddingXY 8 18 ] [])
        ]


makeNewNoteButton model =
    Input.button
        ((Style.select <| model.appMode == UserNotes CreatingNote) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just MakeNewNote
        , label = Element.text "Create"
        }



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
        , setNoteModeButton BrowsingNotes "Notes" model
        , el [ centerX, Font.size 18, Font.color Style.white ] (text <| appTitle model)
        ]


currentUserIsAdmin : Model -> Bool
currentUserIsAdmin model =
    case model.currentUser of
        Nothing ->
            False

        Just user ->
            user.admin


appTitle : Model -> String
appTitle model =
    case model.currentUser of
        Nothing ->
            "Notes"

        Just user ->
            user.username ++ ": Notes"


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


setNoteModeButton : NotesMode -> String -> Model -> Element FrontendMsg
setNoteModeButton notesMode label model =
    Input.button ((Style.select <| model.appMode == UserNotes notesMode) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode <| UserNotes notesMode)
        , label = Element.text label
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
            , showIf (model.appMode == UserNotes CreatingNote) (newNotePanel model)
            , showIf (model.appMode == UserNotes EditingNote) (editNotePanel model)
            , viewNote model.maybeCurrentNote
            ]
        ]


newNotePanel model =
    let
        key =
            "aa" ++ String.fromInt model.counter
    in
    column [ spacing 12, paddingXY 20 0 ]
        [ row [ spacing 12 ] [ newNoteButton, inputNewNoteName model ]
        , Keyed.row [] [ ( key, inputNoteBody model ) ]
        ]


editNotePanel model =
    let
        key =
            "a" ++ String.fromInt model.counter
    in
    column [ spacing 12, paddingXY 20 0 ]
        [ row [ spacing 12 ] [ updateNoteButton, inputChangedSubject model ]
        , Keyed.row [] [ ( key, inputNoteBody model ) ]
        ]


inputNoteBody model =
    Input.multiline (Style.multiline 320 410)
        { onChange = GotNoteBody
        , text = model.noteBody
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        , spellcheck = False
        }


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
        { onPress = Just FECreateNote
        , label = Element.text "Create note"
        }


updateNoteButton : Element FrontendMsg
updateNoteButton =
    Input.button Style.button
        { onPress = Just DoUpdateNote
        , label = Element.text "Save"
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
        , text = model.newSubject
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputChangedSubject model =
    Input.text (Style.inputStyle 250)
        { onChange = GotChangedSubject
        , text = model.changedSubject
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }



--
--
-- VIEWLOGS
--


viewNotes : Model -> Element FrontendMsg
viewNotes model =
    let
        today =
            model.currentTime

        notes =
            Note.bigDateFilter today model.noteCameBeforeString model.noteCameAfterString model.notes
                |> Note.filter model.noteFilterString
                |> Note.filterText model.textFilterString
    in
    column [ spacing 12, padding 20, height (px 430) ]
        [ el [ Font.size 16, Font.bold ] (text "Notes")
        , indexedTable [ spacing 4, Font.size 12, height (px 400), scrollbarY ]
            { data = notes
            , columns =
                [ { header = el [ Font.bold ] (text <| idLabel model)
                  , width = px 30
                  , view = \k note -> el [ Font.size 12 ] (text <| String.fromInt (k + 1))
                  }

                -- , { header = el [ Font.bold ] (text "Date")
                --   , width = px 80
                --   , view = \k note -> el [ Font.size 12 ] (text <| DateTime.humanDateStringFromPosix <| note.timeCreated)
                --   }
                , { header = el [ Font.bold ] (text "Subject")
                  , width = px 220
                  , view = \k note -> selectNoteButton note
                  }
                ]
            }
        , row [ spacing 24, alignBottom, alignLeft ]
            [ el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Count: " ++ String.fromInt (List.length notes))
            ]
        ]


selectNoteButton : Note -> Element FrontendMsg
selectNoteButton note =
    Input.button Style.listItemButton
        { onPress = Just <| SetCurrentNote note
        , label = el [ Font.size 12, Font.color Style.darkBlue ] (text note.subject)
        }


idLabel : Model -> String
idLabel model =
    "id"



--
-- VIEW NOTE
--


noteNotePanel model =
    case model.maybeCurrentNote of
        Nothing ->
            Element.none

        Just evt ->
            column [ width (px 300), height (px 450), padding 12, Border.width 1, spacing 36 ]
                [ el [ Font.bold ] (text <| "Edit note " ++ String.fromInt evt.id)
                , row [ spacing 12 ]
                    [ deleteNoteButton model
                    , showIf (model.deleteNoteSafety == DeleteNoteSafetyOff) cancelDeleteNoteButton
                    ]
                ]



--
--
--


submitNoteButton : Element FrontendMsg
submitNoteButton =
    Input.button Style.button
        { onPress = Just FECreateNote
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
    case ( model.currentUser, String.length model.newSubject > 0 ) of
        ( Just user, True ) ->
            let
                now =
                    model.currentTime
            in
            Just <|
                { id = -1
                , subject = model.newSubject

                --, body = "# " ++ model.newSubject ++ "\n\n" ++ model.noteBody
                , body = model.noteBody
                , timeCreated = now
                , timeModified = now
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
                ]
            }
        ]


viewNote : Maybe Note -> Element FrontendMsg
viewNote maybeNote =
    case maybeNote of
        Nothing ->
            column [ padding 20, spacing 20, height (px 450), width (px 350), Border.width 1 ]
                [ el [ Font.size 12 ] (text "No note selected")
                ]

        Just note ->
            column [ padding 20, spacing 12, height (px 450), width (px 350), Border.width 1 ]
                [ toMarkdown note.body |> Element.html
                ]


filterPanel model =
    row [ spacing 24 ]
        [ row [ spacing 8 ]
            [ el [ Font.bold ] (text "Filter subject:")
            , inputNoteNameFilter model
            ]
        , row [ spacing 8 ]
            [ el [ Font.bold ] (text "Filter body:")
            , inputTextFilter model
            ]
        ]



-- , row [ spacing 8 ]
--     [ el [ Font.bold, Font.size 14 ] (text "After")
--     , displayShiftedDate model.noteCameAfterString model.currentTime
--     ]
-- , inputNoteCameAfterFilter model
-- , row [ spacing 8 ]
--     [ el [ Font.bold, Font.size 14 ] (text "Before")
--     , displayShiftedDate model.noteCameBeforeString model.currentTime
--     ]
-- , inputNoteCameBeforeFilter model
--, row [ alignRight, moveRight 36, spacing 12 ] [ editModeButton sharedState model, noteModeButton model ]


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


inputTextFilter model =
    Input.text (Style.inputStyle 200)
        { onChange = GotTextFilter
        , text = model.textFilterString
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


noteNameButton : Maybe Note -> Note -> Element FrontendMsg
noteNameButton maybeCurrentNote note =
    Input.button (Style.titleButton (maybeCurrentNote == Just note))
        { onPress = Just NoOpFrontendMsg
        , label = Element.text note.subject
        }


setCurrentNoteButton : Model -> Note -> Int -> Element FrontendMsg
setCurrentNoteButton model note index =
    Input.button (Style.titleButton (Just note == model.maybeCurrentNote))
        { onPress = Just NoOpFrontendMsg
        , label = el [ Font.bold ] (Element.text <| String.fromInt note.id)
        }



--
-- UPDATE HELPERS
--


type alias UpdateNoteRecord =
    { maybeCurrentNote : Note
    , notes : List Note
    , cmd : Cmd FrontendMsg
    }



--
-- MARKDOWN
--


myOptions : Markdown.Options
myOptions =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Just "elm"
    , sanitize = False
    , smartypants = False
    }


markdownStyle =
    [ HA.style "font-size" "14px"
    , HA.style "width" "300px"
    , HA.style "overflow-y" "scroll"
    , HA.style "white-space" "normal"
    , HA.style "img" "width: 200px"

    -- , HA.style "p" "display: inline-block"
    ]


toMarkdown : String -> Html FrontendMsg
toMarkdown userInput =
    Markdown.toHtmlWith myOptions markdownStyle userInput



--
-- END
--
