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
import View.UserValidation
import View.UserNotes
import View.Footer
import View.Header
import View.Admin
import Config exposing(config, Config)
import KeyCommands as KC
import Update.Helper


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
                { title = "Lamdera Notes (2b)"
                , body = [ view model ]
                }
        }


type alias Model =
    FrontendModel


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
        [ Time.every 1000 TimeChange
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
                                |> KC.toggleManualUsingKey pressedKeys
                                |> KC.setBrowsingModeUsingKey pressedKeys
                                |> KC.editNoteUsingKey pressedKeys
                                |> KC.makeNewNoteUsingKey pressedKeys
                                |> KC.sortIncreasingByDateUsingKey pressedKeys
                                |> KC.sortDecreasingByDateUsingKey pressedKeys
                                |> KC.sortIncreasingAlphabeticallyUsingKey pressedKeys
                                |> KC.unsortedUsingKey pressedKeys

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
              case Utility.currentUserIsAdmin model of
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
                    Update.Helper.selectNotes model
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
                    Update.Helper.selectNotes model
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
            Update.Helper.createNote model

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
        , View.Header.view model
        , case model.appMode of
            UserValidation _ ->
                View.UserValidation.view model

            UserNotes _ ->
                View.UserNotes.view model

            Admin ->
                View.Admin.view model
        , View.Footer.view model
        ]

--
-- UPDATE HELPERS
--


type alias UpdateNoteRecord =
    { maybeCurrentNote : Note
    , notes : List Note
    , cmd : Cmd FrontendMsg
    }

--
-- createNote : Model -> ( Model, Cmd FrontendMsg )
-- createNote model =
--     case newNote model of
--         Nothing ->
--             ( model, Cmd.none )
--
--         Just note ->
--             ( { model
--                 | maybeCurrentNote = Just note
--                 , changedSubject = note.subject
--                 , noteBody = note.body
--                 , appMode = UserNotes EditingNote
--                 , counter = model.counter + 1
--               }
--             , sendToBackend (CreateNote model.currentUser note)
--             )
--
--
-- makeNewNote : Model -> Model
-- makeNewNote model =
--     let
--         n =
--             Note.make "dkjfldsjfldjf-dfjldf" "New Note" "XXX" model.currentTime
--     in
--     { model
--         | appMode = UserNotes CreatingNote
--         , maybeCurrentNote = Just n
--         , noteBody = n.body
--         , newSubject = n.subject
--         , tagString = ""
--         , counter = model.counter + 1
--     }
--
--
-- editNote : Model -> Model
-- editNote model =
--     case model.maybeCurrentNote of
--         Nothing ->
--             model
--
--         Just note ->
--             { model
--                 | appMode = UserNotes EditingNote
--                 , noteBody = note.body
--                 , changedSubject = note.subject
--                 , tagString = String.join ", " note.tags
--             }
--
--
-- newNote : Model -> Maybe Note
-- newNote model =
--     case model.currentUser of
--         Just user ->
--             let
--                 now =
--                     model.currentTime
--             in
--             Just <|
--                 { id = "---"
--                 , subject = "New Note"
--                 , body = ""
--                 , tags = Note.tagsFromString model.tagString
--                 , timeCreated = now
--                 , timeModified = now
--                 , selected = True
--                 }
--
--         _ ->
--             Nothing
--
--
-- selectNotes model =
--     Note.bigDateFilter model.currentTime model.noteCameBeforeString model.noteCameAfterString model.notes
--         |> Note.applySubjectFilter model.noteFilterString
--         |> Note.applyBodyFilter model.textFilterString
--         |> Note.applyTagFilter model.tagFilterString
--
--
--


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
-- DOWNLOAD
--


downloadNotes : List Note -> Cmd FrontendMsg
downloadNotes noteList =
    Download.string "notes.yaml" "text/yaml" (Note.listToYaml noteList)





pxString : Float -> String
pxString f =
    String.fromFloat f ++ "px"


pxFloat =
    round >> px



--
-- END
--
