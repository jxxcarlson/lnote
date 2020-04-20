
module View.Header exposing(view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Types exposing(DeleteNoteSafety(..), NotesMode(..), FrontendModel, FrontendMsg(..), ValidationState(..), AppMode(..))
import Style
import View.Utility exposing(showIf)
import Config exposing(config)
import User exposing(User)
import Note exposing(Note)
import DateTime
import FrequencyDict
import Time exposing(Posix)
import Text
import Html exposing(Html)
import View.Button
import View.Utility
import Utility

type alias Model = FrontendModel

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
