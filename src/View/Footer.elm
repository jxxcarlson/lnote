module View.Footer exposing(view)


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



--
-- FOOTER
--


view model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            activeFooter model


blankFooter model =
    row [ width fill, spacing 18, Background.color Style.charcoal, paddingXY 8 18 ]
        [ el [ Font.color Style.white, Font.size 12 ] (text model.message)
        ]


activeFooter model =
    row [ width fill, spacing 18, Background.color Style.charcoal, paddingXY 8 8 ]
        [ el [ Font.bold, Font.color Style.white ] (text "Note:")
        , View.Button.setNoteMode BrowsingNotes "Browse" model
        , View.Utility.hideIf (model.currentUser == Nothing) (editNoteButton model)
        , makeNewNoteButton model
        , row [ paddingXY 24 0 ] [ showIf (model.maybeCurrentNote /= Nothing) (View.Button.deleteNote model) ]
        , View.Utility.hideIf (model.currentUser == Nothing) View.Button.download
        , View.Button.toggleManual model
        , el [ Font.color Style.white, Font.size 12 ] (text model.message)
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
