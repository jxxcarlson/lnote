module View.Header exposing (view)

import Config exposing (config)
import DateTime
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FrequencyDict
import Html exposing (Html)
import Note exposing (Note)
import Style
import Text
import Time exposing (Posix)
import Types exposing (AppMode(..), DeleteNoteSafety(..), FrontendModel, FrontendMsg(..), NotesMode(..), ValidationState(..))
import User exposing (User)
import Utility
import View.Button
import View.Utility exposing (showIf)


type alias Model =
    FrontendModel


view : Model -> Element FrontendMsg
view model =
    row
        [ width fill
        , paddingXY 40 8
        , Background.color Style.charcoal
        , spacing 12
        ]
        [ showIf (Utility.currentUserIsAdmin model) (View.Button.adminMode model)
        , View.Button.userValidationMode model
        , View.Button.setNoteMode BrowsingNotes "Notes" model
        , el [ centerX, Font.size 18, Font.color Style.white ] (text <| appTitle model)
        ]


appTitle : Model -> String
appTitle model =
    case model.currentUser of
        Nothing ->
            "Notes"

        Just user ->
            user.username ++ ": Notes"
