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
import Debounce exposing (Debounce)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import File.Download as Download
import FrequencyDict exposing (FrequencyDict)
import Graph exposing (Option(..))
import Html exposing (Html, time)
import Html.Attributes as HA
import Keyboard exposing (Key(..))
import Lamdera exposing (sendToBackend)
import Markdown
import Note exposing (Note)
import Random
import Style
import Task
import TestData exposing (..)
import Text
import Time exposing (Posix)
import TypedTime exposing (..)
import Types exposing (AppMode(..), BackendMsg(..), DeleteNoteSafety(..), FrontendModel, FrontendMsg(..), NotesMode(..), ToBackend(..), ToFrontend(..), ValidationState(..))
import UUID
import Url exposing (Url)
import User exposing (User, Username)
import Utility


app =
    Lamdera.frontend
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
-- Types
--
--
-- MODEL
--


type alias Model =
    FrontendModel



--
-- INIT
--
--
-- CONFIG
--


type alias Config =
    { debounceInterval : Float
    , timeoutInMs : Int
    , panelHeight : Float
    , panelWidth : Float
    }


config : Config
config =
    { debounceInterval = 500
    , timeoutInMs = 5 * 1000
    , panelHeight = 550
    , panelWidth = 450
    }


initialModel : Model
initialModel =
    { input = "App started"
    , message = "Please sign in"
    , pressedKeys = []
    , appMode = UserValidation SignInState
    , manualVisible = False
    , currentTime = Time.millisToPosix 0
    , counter = 0
    , bodyDebouncer = Debounce.init
    , subjectDebouncer = Debounce.init
    , tagDebouncer = Debounce.init

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
    , frequencyDict = Dict.empty
    , noteBody = ""
    , tagString = ""
    , noteFilterString = ""
    , newSubject = ""
    , changedSubject = ""
    , textFilterString = ""
    , tagFilterString = ""
    , deleteNoteSafety = DeleteNoteSafetyOn
    , noteCameBeforeString = ""
    , noteCameAfterString = ""
    , uuid = Nothing
    }


init : ( Model, Cmd FrontendMsg )
init =
    ( initialModel, sendToBackend ClientJoin )


subscriptions model =
    Sub.batch
        [ Time.every 30000 TimeChange
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]



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
            ( { model
                | notes = Note.selectAll newNoteList
                , maybeCurrentNote = List.head newNoteList
              }
            , Cmd.none
            )

        SendNoteToFrontend note ->
            ( { model
                | notes = Note.replace (Note.select note) model.notes
                , maybeCurrentNote = Just (Note.select note)
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
                    , sendToBackend (RequestNotes (Just user))
                    )

        SendFrequencyDict fD ->
            ( { model | frequencyDict = fD }, Cmd.none )


bodyDebounceConfig : Debounce.Config FrontendMsg
bodyDebounceConfig =
    { strategy = Debounce.later config.debounceInterval
    , transform = DebounceBody
    }


subjectDebounceConfig : Debounce.Config FrontendMsg
subjectDebounceConfig =
    { strategy = Debounce.later config.debounceInterval
    , transform = DebounceSubject
    }


tagDebounceConfig : Debounce.Config FrontendMsg
tagDebounceConfig =
    { strategy = Debounce.later config.debounceInterval
    , transform = DebounceTags
    }


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        FENoop ->
            ( model, Cmd.none )

        Types.KeyboardMsg keyMsg ->
            let
                pressedKeys =
                    Keyboard.update keyMsg model.pressedKeys

                newModel =
                    case List.member Control pressedKeys of
                        False ->
                            model

                        True ->
                            model
                                |> toggleManualUsingKey pressedKeys
                                |> setBrowsingModeUsingKey pressedKeys
                                |> editNoteUsingKey pressedKeys
                                |> makeNewNoteUsingKey pressedKeys
                                |> sortIncreasingByDateUsingKey pressedKeys
                                |> sortDecreasingByDateUsingKey pressedKeys
                                |> sortIncreasingAlphabeticallyUsingKey pressedKeys
                                |> unsortedUsingKey pressedKeys

                cmd =
                    case List.member Control pressedKeys of
                        False ->
                            Cmd.none

                        True ->
                            if List.member (Character "R") pressedKeys then
                                Random.generate SelectRandomNotes (Utility.randomIntegers 10 (List.length model.notes))

                            else
                                Cmd.none
            in
            ( { newModel
                | pressedKeys = pressedKeys
              }
            , cmd
            )

        SetManualVislble bit ->
            ( { model | manualVisible = bit }, Cmd.none )

        DebounceBody msg_ ->
            let
                save =
                    case ( model.maybeCurrentNote, model.appMode ) of
                        ( Just note, UserNotes EditingNote ) ->
                            \s -> sendToBackend (UpdateNote model.currentUser { note | body = s, timeModified = model.currentTime })

                        _ ->
                            \s -> Cmd.none

                ( debounce, cmd ) =
                    Debounce.update
                        bodyDebounceConfig
                        (Debounce.takeLast save)
                        msg_
                        model.bodyDebouncer
            in
            ( model, cmd )

        DebounceSubject msg_ ->
            let
                save =
                    case ( model.maybeCurrentNote, model.appMode ) of
                        ( Just note, UserNotes EditingNote ) ->
                            \s -> sendToBackend (UpdateNote model.currentUser { note | subject = s, timeModified = model.currentTime })

                        _ ->
                            \s -> Cmd.none

                ( debounce, cmd ) =
                    Debounce.update
                        subjectDebounceConfig
                        (Debounce.takeLast save)
                        msg_
                        model.subjectDebouncer
            in
            ( model, cmd )

        DebounceTags msg_ ->
            let
                save =
                    case ( model.maybeCurrentNote, model.appMode ) of
                        ( Just note, UserNotes EditingNote ) ->
                            \s -> sendToBackend (UpdateTags model.currentUser { note | tags = Note.tagsFromString s, timeModified = model.currentTime })

                        _ ->
                            \s -> Cmd.none

                ( debounce, cmd ) =
                    Debounce.update
                        tagDebounceConfig
                        (Debounce.takeLast save)
                        msg_
                        model.tagDebouncer
            in
            ( model, cmd )

        TimeChange t ->
            ( { model | currentTime = t }, Cmd.none )

        -- ADMIN --
        SendUsers ->
            case currentUserIsAdmin model of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( model
                    , sendToBackend RequestUsers
                    )

        -- BACKEND
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
                            sendToBackend RequestUsers

                        _ ->
                            Cmd.none

                newModel =
                    case mode of
                        UserValidation SignInState ->
                            { model | message = "Please sign in", appMode = mode }

                        UserValidation SignUpState ->
                            { model | message = "Please sign up", appMode = mode }

                        UserNotes BrowsingNotes ->
                            { model | appMode = mode, maybeCurrentNote = Note.firstSelectedNote model.notes }

                        _ ->
                            { model | appMode = mode }
            in
            ( newModel, cmd )

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
                            , sendToBackend (SendChangePasswordInfo user.username model.password model.newPassword1)
                            )

                errorList ->
                    ( { model | message = String.join ", " errorList }, Cmd.none )

        GotEmail str ->
            ( { model | email = str }, Cmd.none )

        SignIn ->
            ( initialModel, sendToBackend (SendSignInInfo model.username model.password) )

        SignUp ->
            let
                signUpErrors =
                    User.validateSignUpInfo model.username model.password model.email
            in
            case List.length signUpErrors > 0 of
                True ->
                    ( { model | message = String.join ", " signUpErrors }, Cmd.none )

                False ->
                    ( initialModel, sendToBackend (SendSignUpInfo model.username model.password model.email) )

        SignOut ->
            ( initialModel, Cmd.none )

        -- NOTES --
        DownloadNotes ->
            ( model, downloadNotes (List.filter (\note -> note.selected) model.notes) )

        SetCurrentNote note ->
            ( { model
                | maybeCurrentNote = Just note
                , changedSubject = note.subject
                , noteBody = note.body
                , tagString = String.join ", " note.tags
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        GotNoteFilter str ->
            let
                selectedNotes =
                    Note.applySubjectFilter str model.notes

                maybeCurrentNote =
                    Note.firstSelectedNote selectedNotes

                newModel =
                    case maybeCurrentNote of
                        Nothing ->
                            model

                        Just note ->
                            { model
                                | noteBody = note.body
                                , changedSubject = note.subject
                                , tagString = String.join ", " note.tags
                            }
            in
            ( { newModel
                | noteFilterString = str
                , notes = selectedNotes
                , maybeCurrentNote = maybeCurrentNote
              }
            , Cmd.none
            )

        GotTextFilter str ->
            let
                selectedNotes =
                    Note.applyBodyFilter str model.notes

                maybeCurrentNote =
                    Note.firstSelectedNote selectedNotes

                newModel =
                    case maybeCurrentNote of
                        Nothing ->
                            model

                        Just note ->
                            { model
                                | noteBody = note.body
                                , changedSubject = note.subject
                                , tagString = String.join ", " note.tags
                            }
            in
            ( { newModel
                | textFilterString = str
                , notes = selectedNotes
                , maybeCurrentNote = maybeCurrentNote
              }
            , Cmd.none
            )

        GotTagFilter str ->
            let
                selectedNotes =
                    Note.applyTagFilter str model.notes

                maybeCurrentNote =
                    Note.firstSelectedNote selectedNotes

                newModel =
                    case maybeCurrentNote of
                        Nothing ->
                            model

                        Just note ->
                            { model
                                | noteBody = note.body
                                , changedSubject = note.subject
                                , tagString = String.join ", " note.tags
                            }
            in
            ( { newModel
                | tagFilterString = str
                , notes = selectedNotes
                , maybeCurrentNote = maybeCurrentNote
              }
            , Cmd.none
            )

        SetTagForSearch tag ->
            let
                newNotes =
                    Note.applyTagFilter tag model.notes

                maybeCurrentNote =
                    Note.firstSelectedNote newNotes

                newModel =
                    case maybeCurrentNote of
                        Nothing ->
                            { model | notes = newNotes, maybeCurrentNote = Nothing }

                        Just note ->
                            { model
                                | notes = newNotes
                                , maybeCurrentNote = Just note
                                , noteBody = note.body
                                , changedSubject = note.subject
                                , tagString = String.join ", " note.tags
                                , tagFilterString = tag
                            }
            in
            ( newModel, Cmd.none )

        ClearAllSearches ->
            ( { model
                | notes = Note.selectAll model.notes
                , noteFilterString = ""
                , textFilterString = ""
                , tagFilterString = ""
              }
            , Cmd.none
            )

        GotNoteDateAfterFilter str ->
            let
                newNotes =
                    selectNotes model
            in
            ( { model
                | noteCameAfterString = str
                , notes = newNotes
                , maybeCurrentNote = Note.firstSelectedNote newNotes
              }
            , Cmd.none
            )

        GotNoteDateBeforeFilter str ->
            let
                newNotes =
                    selectNotes model
            in
            ( { model
                | noteCameBeforeString = str
                , notes = newNotes
                , maybeCurrentNote = Note.firstSelectedNote newNotes
              }
            , Cmd.none
            )

        GetRandomNotes ->
            ( model, Random.generate SelectRandomNotes (Utility.randomIntegers 10 (List.length model.notes)) )

        SelectRandomNotes randomInts ->
            let
                newNotes =
                    Note.selectSublist randomInts model.notes
            in
            ( { model | notes = newNotes, maybeCurrentNote = Note.firstSelectedNote newNotes }, Cmd.none )

        MakeNewNote ->
            createNote model

        FEEditNote ->
            case model.maybeCurrentNote of
                Nothing ->
                    ( model, Cmd.none )

                Just note ->
                    ( { model
                        | appMode = UserNotes EditingNote
                        , noteBody = note.body
                        , changedSubject = note.subject
                        , tagString = String.join ", " note.tags
                      }
                    , Cmd.none
                    )

        DoUpdateNote ->
            -- YYY
            case model.maybeCurrentNote of
                Nothing ->
                    ( model, Cmd.none )

                Just note ->
                    let
                        updatedNote =
                            { note
                                | subject = model.changedSubject
                                , body = model.noteBody
                                , timeModified = model.currentTime
                                , tags = Note.tagsFromString model.tagString
                            }
                    in
                    ( { model
                        | maybeCurrentNote = Just updatedNote
                        , changedSubject = updatedNote.subject
                        , newSubject = updatedNote.subject
                        , noteBody = updatedNote.body

                        -- , notes = Note.replace updatedNote model.notes
                      }
                    , sendToBackend (UpdateNote model.currentUser updatedNote)
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
                    , sendToBackend (DeleteNote model.currentUser note)
                    )

        GotNewNoteName str ->
            ( { model | newSubject = str }, Cmd.none )

        GotTagString tagString ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        tagDebounceConfig
                        tagString
                        model.tagDebouncer
            in
            ( { model
                | tagDebouncer = newDebouncer
                , tagString = tagString
              }
            , cmd
            )

        GotNoteBody noteBody ->
            -- YYY
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        bodyDebounceConfig
                        noteBody
                        model.bodyDebouncer
            in
            ( { model
                | noteBody = noteBody
                , bodyDebouncer = newDebouncer
              }
            , cmd
            )

        GotChangedSubject subject ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        subjectDebounceConfig
                        subject
                        model.subjectDebouncer
            in
            ( { model
                | subjectDebouncer = newDebouncer
                , changedSubject = subject
              }
            , cmd
            )



-- sendToBackend (SendSignInInfo model.username model.password) )
--
-- VIEW
--


view : Model -> Html FrontendMsg
view model =
    Element.layout [] (mainView model)


mainView : Model -> Element FrontendMsg
mainView model =
    column [ height fill ]
        [ Html.node "link" [ HA.rel "stylesheet", HA.href "mystyle.css" ] [] |> Element.html
        , header model
        , case model.appMode of
            UserValidation _ ->
                userValidationView model

            UserNotes _ ->
                masterView model

            Admin ->
                adminView model
        , footer model
        ]



--
-- FOOTER
--


footer model =
    case model.currentUser of
        Nothing ->
            blankFooter

        Just _ ->
            activeFooter model


blankFooter =
    row [ width fill, spacing 18, Background.color Style.charcoal, paddingXY 8 18 ] []


activeFooter model =
    row [ width fill, spacing 18, Background.color Style.charcoal, paddingXY 8 8 ]
        [ el [ Font.bold, Font.color Style.white ] (text "Note:")
        , setNoteModeButton BrowsingNotes "Browse" model
        , hideIf (model.currentUser == Nothing) (editNoteButton model)
        , makeNewNoteButton model
        , row [ paddingXY 24 0 ] [ showIf (model.maybeCurrentNote /= Nothing) (deleteNoteButton model) ]
        , hideIf (model.currentUser == Nothing) downloadButton
        , toggleManualButton model
        ]


makeNewNoteButton model =
    Input.button
        ((Style.select <| model.appMode == UserNotes CreatingNote) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just MakeNewNote
        , label = Element.text "New"
        }


editNoteButton model =
    Input.button
        ((Style.select <| model.appMode == UserNotes EditingNote) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just FEEditNote
        , label = Element.text "Edit"
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
    row [ spacing 12 ]
        [ noUserLHS model
        , noUserRHS model
        ]


noUserLHS model =
    column Style.mainColumnX
        [ el [ Font.size 18, Font.bold, paddingXY 0 12 ] (text "Welcome to Lamdera Notes")
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
        , el [ Font.size 16, Font.color Style.darkRed ] (text model.message)
        ]


pxFloat =
    round >> px


noUserRHS model =
    column [ padding 40, spacing 18, Font.size 16 ]
        [ el [ Font.bold, Font.size 24 ]
            (text "Screenshot of app")
        , image
            [ width (pxFloat config.panelWidth) ]
            { src = "http://noteimages.s3.amazonaws.com/jim_images/notes-screen.png"
            , description = "screenshot of app"
            }
        , Element.paragraph []
            [ el [ Font.bold ] (text "Features. ")
            , text "Searchable note repository. Supports Markdown. Filter notes by title, tags, full text. "
            , text "Active links to the most-used tags. Notes are automatically saved every 0.5 second."
            ]
        , Element.paragraph []
            [ el [ Font.bold ] (text "Coming soon. ")
            , text "Filter by date, options to sort note list; export."
            ]
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
        , el [ Font.size 16, Font.color Style.darkRed ] (text model.message)
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


randomNotesButton =
    Input.button
        Style.headerButton
        { onPress = Just GetRandomNotes
        , label = Element.text "Random"
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
                    Just FENoop
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
                    Just FENoop
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
            , showIf model.manualVisible manual
            ]
        ]


newNotePanel model =
    let
        key1 =
            "aa" ++ String.fromInt model.counter

        key2 =
            "bb" ++ String.fromInt model.counter
    in
    column [ spacing 12, paddingXY 20 0 ]
        [ row [ spacing 12 ] [ newNoteButton, inputNewNoteName model ]
        , Keyed.row [] [ ( key1, inputNoteBody model ) ]
        , Keyed.row [] [ ( key2, inputNoteTags model ) ]
        ]


editNotePanel model =
    let
        key1 =
            "a" ++ String.fromInt model.counter

        key2 =
            "aa" ++ String.fromInt model.counter
    in
    column [ spacing 12, paddingXY 20 0 ]
        [ row [ spacing 12 ] [ updateNoteButton, inputChangedSubject model ]
        , Keyed.row [] [ ( key1, inputNoteBody model ) ]
        , Keyed.row [] [ ( key2, inputNoteTags model ) ]
        ]


inputNoteBody model =
    Input.multiline (Style.multiline 400 (round (config.panelHeight - 85)))
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
        { onPress = Just MakeNewNote
        , label = Element.text "Create note"
        }


updateNoteButton : Element FrontendMsg
updateNoteButton =
    Input.button Style.button
        { onPress = Just FENoop
        , label = Element.text "Subject"
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
    Input.text (Style.inputStyle 285)
        { onChange = GotNewNoteName
        , text = model.newSubject
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputNoteTags model =
    Input.text (Style.inputStyle 365)
        { onChange = GotTagString
        , text = model.tagString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "Tags: ")
        }


inputChangedSubject model =
    Input.text (Style.inputStyle 315)
        { onChange = GotChangedSubject
        , text = model.changedSubject
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }



--
--
-- VIEWLOGS
--


noteListPanel : Model -> Element FrontendMsg
noteListPanel model =
    column [ spacing 20, height (pxFloat config.panelHeight), width (pxFloat <| 0.9 * config.panelWidth - 50), Border.width 1 ]
        [ viewNotes model
        ]


noteNameButton : Maybe Note -> Note -> Element FrontendMsg
noteNameButton maybeCurrentNote note =
    Input.button (Style.titleButton (maybeCurrentNote == Just note))
        { onPress = Just FENoop
        , label = Element.text note.subject
        }


setCurrentNoteButton : Model -> Note -> Int -> Element FrontendMsg
setCurrentNoteButton model note index =
    Input.button (Style.titleButton (Just note == model.maybeCurrentNote))
        { onPress = Just FENoop
        , label = el [ Font.bold ] (Element.text <| String.slice 0 4 note.id)
        }


viewNotes : Model -> Element FrontendMsg
viewNotes model =
    column [ spacing 12, padding 20, height (px 510) ]
        [ el [ Font.size 16, Font.bold ] (text "Notes")
        , indexedTable [ spacing 4, Font.size 12, height (pxFloat (config.panelHeight - 50)), scrollbarY ]
            { data = List.filter (\note -> note.selected) model.notes
            , columns =
                [ { header = el [ Font.bold ] (text <| idLabel model)
                  , width = px 36
                  , view = \k note -> el [ Font.size 12, Font.family [ Font.typeface "Courier New", Font.monospace ] ] (text <| String.slice 0 4 note.id)
                  }
                , { header = el [ Font.bold ] (text <| "Modified")
                  , width = px 90
                  , view = \k note -> el [ Font.size 12 ] (text <| DateTime.humanDateStringFromPosix note.timeModified)
                  }
                , { header = el [ Font.bold ] (text "Subject")
                  , width = px 220
                  , view = \k note -> selectNoteButton model.maybeCurrentNote note
                  }
                ]
            }
        , column [ spacing 8 ]
            [ row [ spacing 24, alignBottom, alignLeft ]
                [ el [ Font.size 14, Font.bold ] (text <| "Count: " ++ String.fromInt (List.length (List.filter (\note -> note.selected) model.notes)))
                ]
            , tagButtons model
            ]
        ]


viewNote : Maybe Note -> Element FrontendMsg
viewNote maybeNote =
    case maybeNote of
        Nothing ->
            column [ padding 20, spacing 20, height (pxFloat config.panelHeight), width (pxFloat config.panelWidth), Border.width 1 ]
                [ el [ Font.size 12 ] (text "No note selected")
                ]

        Just note ->
            let
                created =
                    "*Created: " ++ DateTime.humanDateStringFromPosix note.timeCreated ++ "*, "

                modified =
                    "*modified: " ++ DateTime.humanDateStringFromPosix note.timeModified ++ "*\n\n"

                title =
                    "# " ++ note.subject ++ "\n\n"

                words =
                    "**Words:** " ++ String.fromInt (Note.wordCount note) ++ " (" ++ String.fromInt (String.length note.body) ++ " characters)" ++ "\n\n"

                tags =
                    case note.tags of
                        [] ->
                            "Tags: none\n\n"

                        _ ->
                            "**Tags:** " ++ String.join ", " note.tags ++ "\n\n"

                hr =
                    """<hr style="height:1px; border:none; background-color: #333;" />

"""

                content =
                    title ++ created ++ modified ++ words ++ tags ++ hr ++ note.body
            in
            column [ padding 20, spacing 12, height (pxFloat config.panelHeight), width (pxFloat config.panelWidth), Border.width 1 ]
                [ toMarkdown content |> Element.html
                ]


tagButtons : Model -> Element FrontendMsg
tagButtons model =
    model.frequencyDict
        |> FrequencyDict.list
        |> List.map (\item -> tagButton item)
        |> List.take 20
        |> (\x -> el [ Font.size 12 ] (text "Tags:") :: x)
        |> (\x -> x ++ [ clearTagSearch ])
        |> (\x -> Element.paragraph [ spacing 8, Font.size 14, width (pxFloat (2 * config.panelWidth - 60)) ] x)


tagButton : ( String, Int ) -> Element FrontendMsg
tagButton ( tag, freq ) =
    Input.button (Style.titleButton False ++ [ paddingXY 4 0 ])
        { onPress = Just (SetTagForSearch tag)
        , label = text (tag ++ ": " ++ String.fromInt freq)
        }


clearTagSearch : Element FrontendMsg
clearTagSearch =
    Input.button Style.smallButton
        { onPress = Just ClearAllSearches
        , label = text "Clear tag search"
        }


clearAllSearches : Element FrontendMsg
clearAllSearches =
    Input.button Style.smallButton
        { onPress = Just ClearAllSearches
        , label = text "Clear searches"
        }


selectNoteButton : Maybe Note -> Note -> Element FrontendMsg
selectNoteButton maybeCurrentNote note =
    Input.button (Style.titleButton (maybeCurrentNote == Just note))
        { onPress = Just (SetCurrentNote note)
        , label = text note.subject
        }


idLabel : Model -> String
idLabel model =
    "id"



--
-- VIEW NOTE
--


submitNoteButton : Element FrontendMsg
submitNoteButton =
    Input.button Style.button
        { onPress = Just MakeNewNote
        , label = Element.text "New: minutes or hh:mm"
        }



--
-- UPDATE HELPERS
--


type alias UpdateNoteRecord =
    { maybeCurrentNote : Note
    , notes : List Note
    , cmd : Cmd FrontendMsg
    }


createNote : Model -> ( Model, Cmd FrontendMsg )
createNote model =
    case newNote model of
        Nothing ->
            ( model, Cmd.none )

        Just note ->
            ( { model
                | maybeCurrentNote = Just note
                , changedSubject = note.subject
                , noteBody = note.body
                , appMode = UserNotes EditingNote
                , counter = model.counter + 1
              }
            , sendToBackend (CreateNote model.currentUser note)
            )


makeNewNote : Model -> Model
makeNewNote model =
    let
        n =
            Note.make "dkjfldsjfldjf-dfjldf" "New Note" "XXX" model.currentTime
    in
    { model
        | appMode = UserNotes CreatingNote
        , maybeCurrentNote = Just n
        , noteBody = n.body
        , newSubject = n.subject
        , tagString = ""
        , counter = model.counter + 1
    }


editNote : Model -> Model
editNote model =
    case model.maybeCurrentNote of
        Nothing ->
            model

        Just note ->
            { model
                | appMode = UserNotes EditingNote
                , noteBody = note.body
                , changedSubject = note.subject
                , tagString = String.join ", " note.tags
            }


newNote : Model -> Maybe Note
newNote model =
    case model.currentUser of
        Just user ->
            let
                now =
                    model.currentTime
            in
            Just <|
                { id = "---"
                , subject = "New Note"
                , body = ""
                , tags = Note.tagsFromString model.tagString
                , timeCreated = now
                , timeModified = now
                , selected = True
                }

        _ ->
            Nothing


selectNotes model =
    Note.bigDateFilter model.currentTime model.noteCameBeforeString model.noteCameAfterString model.notes
        |> Note.applySubjectFilter model.noteFilterString
        |> Note.applyBodyFilter model.textFilterString
        |> Note.applyTagFilter model.tagFilterString



--
-- KEY COMMANDS
--


sortIncreasingAlphabeticallyUsingKey : List Key -> Model -> Model
sortIncreasingAlphabeticallyUsingKey pressedKeys model =
    if List.member (Character "A") pressedKeys then
        { model | notes = Note.sortAlphabetically Note.SortIncreasing model.notes }

    else
        model


sortDecreasingAlphabeticallyUsingKey : List Key -> Model -> Model
sortDecreasingAlphabeticallyUsingKey pressedKeys model =
    if List.member (Character "B") pressedKeys then
        { model | notes = Note.sortAlphabetically Note.SortDecreasing model.notes }

    else
        model


sortDecreasingByDateUsingKey : List Key -> Model -> Model
sortDecreasingByDateUsingKey pressedKeys model =
    if List.member (Character "D") pressedKeys then
        { model | notes = Note.sortByTimeModified Note.SortDecreasing model.notes }

    else
        model


sortIncreasingByDateUsingKey : List Key -> Model -> Model
sortIncreasingByDateUsingKey pressedKeys model =
    if List.member (Character "I") pressedKeys then
        { model | notes = Note.sortByTimeModified Note.SortIncreasing model.notes }

    else
        model


unsortedUsingKey : List Key -> Model -> Model
unsortedUsingKey pressedKeys model =
    if List.member (Character "U") pressedKeys then
        { model | notes = Note.selectAll model.notes }

    else
        model


setBrowsingModeUsingKey : List Key -> Model -> Model
setBrowsingModeUsingKey pressedKeys model =
    if List.member (Character "B") pressedKeys then
        { model | appMode = UserNotes BrowsingNotes, maybeCurrentNote = Note.firstSelectedNote model.notes }

    else
        model


toggleManualUsingKey : List Key -> Model -> Model
toggleManualUsingKey pressedKeys model =
    if List.member (Character "M") pressedKeys then
        { model | manualVisible = manualVisibleForKey pressedKeys model }

    else
        model


editNoteUsingKey : List Key -> Model -> Model
editNoteUsingKey pressedKeys model =
    if List.member (Character "E") pressedKeys then
        editNote model

    else
        model


makeNewNoteUsingKey : List Key -> Model -> Model
makeNewNoteUsingKey pressedKeys model =
    if List.member (Character "N") pressedKeys then
        makeNewNote model

    else
        model


appModeOfKey : AppMode -> List Key -> AppMode
appModeOfKey currentAppMode pressedKeys =
    if List.member (Character "B") pressedKeys then
        UserNotes BrowsingNotes

    else
        currentAppMode


manualVisibleForKey : List Key -> Model -> Bool
manualVisibleForKey pressedKeys model =
    if List.member (Character "M") pressedKeys then
        not model.manualVisible

    else
        model.manualVisible



--
-- ELEMENT VISIBILITY HANDLERS
--


showIf : Bool -> Element FrontendMsg -> Element FrontendMsg
showIf bit element =
    if bit then
        element

    else
        Element.none


hideIf : Bool -> Element FrontendMsg -> Element FrontendMsg
hideIf bit element =
    if not bit then
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
                  , width = px 130
                  , view = \k usr -> el [ Font.size 12 ] (text usr.username)
                  }
                , { header = el [ Font.bold ] (text "Email")
                  , width = px 200
                  , view = \k usr -> el [ Font.size 12 ] (text usr.email)
                  }
                ]
            }
        ]



--
-- FILTERS
--


filterPanel model =
    row [ spacing 12 ]
        [ el [ Font.bold ] (text "Filter by")
        , row [ spacing 8 ]
            [ el [ Font.bold ] (text "subject:")
            , inputNoteNameFilter model
            ]
        , row [ spacing 8 ]
            [ el [ Font.bold ] (text "text:")
            , inputTextFilter model
            ]
        , row [ spacing 8 ]
            [ el [ Font.bold ] (text "tag:")
            , inputTagFilter model
            ]
        , clearAllSearches
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
    Input.text (Style.inputStyle 160)
        { onChange = GotNoteFilter
        , text = model.noteFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputTextFilter model =
    Input.text (Style.inputStyle 160)
        { onChange = GotTextFilter
        , text = model.textFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputTagFilter model =
    Input.text (Style.inputStyle 80)
        { onChange = GotTagFilter
        , text = model.tagFilterString
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



--
-- DOWNLOAD
--


downloadNotes : List Note -> Cmd FrontendMsg
downloadNotes noteList =
    Download.string "notes.yaml" "text/yaml" (Note.listToYaml noteList)


downloadButton : Element FrontendMsg
downloadButton =
    Input.button Style.headerButton
        { onPress = Just DownloadNotes
        , label = el [] (text "Download")
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
    , HA.style "width" (pxString <| config.panelWidth - 40)
    , HA.style "overflow-y" "scroll"
    , HA.style "white-space" "normal"
    , HA.style "line-height" "1.4"
    , HA.class "hr-thin"
    , HA.class "image"

    -- , HA.style "p" "display: inline-block"
    ]


pxString : Float -> String
pxString f =
    String.fromFloat f ++ "px"


toMarkdown : String -> Html FrontendMsg
toMarkdown userInput =
    Markdown.toHtmlWith myOptions markdownStyle userInput



--
-- MANUAL
--


viewText : String -> Element FrontendMsg
viewText str =
    column
        [ width (pxFloat config.panelWidth)
        , height (pxFloat config.panelHeight)
        , padding 20
        , scrollbarY
        , Border.width 1
        ]
        [ toMarkdown str |> Element.html ]


manual : Element FrontendMsg
manual =
    viewText Text.manual


toggleManualButton : Model -> Element FrontendMsg
toggleManualButton model =
    case model.manualVisible of
        True ->
            Input.button Style.button
                { onPress = Just (SetManualVislble False)
                , label = Element.text "Hide Manual "
                }

        False ->
            Input.button Style.button
                { onPress = Just (SetManualVislble True)
                , label = Element.text "Show Manual "
                }



--
-- END
--
