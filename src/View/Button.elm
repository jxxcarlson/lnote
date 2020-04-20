module View.Button exposing(deleteNote, userValidationMode, adminMode, setNoteMode, toggleManual, download)


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Types exposing(DeleteNoteSafety(..), NotesMode(..), FrontendModel, FrontendMsg(..), ValidationState(..), AppMode(..))
import Style


type alias Model = FrontendModel

deleteNote : Model -> Element FrontendMsg
deleteNote model =
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



toggleManual : Model -> Element FrontendMsg
toggleManual model =
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


download : Element FrontendMsg
download =
    Input.button Style.headerButton
        { onPress = Just DownloadNotes
        , label = el [] (text "Download")
        }


userValidationMode : Model -> Element FrontendMsg
userValidationMode model =
    Input.button ((Style.select <| model.appMode == UserValidation SignInState) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode (UserValidation SignInState))
        , label = Element.text "User"
        }


adminMode: Model -> Element FrontendMsg
adminMode model =
    Input.button ((Style.select <| model.appMode == Admin) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode Admin)
        , label = Element.text "Admin"
        }


setNoteMode : NotesMode -> String -> Model -> Element FrontendMsg
setNoteMode notesMode label model =
    Input.button ((Style.select <| model.appMode == UserNotes notesMode) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode <| UserNotes notesMode)
        , label = Element.text label
        }
