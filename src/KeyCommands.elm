
module KeyCommands exposing(..)

import Types exposing (FrontendModel, NotesMode(..), AppMode(..))
import Keyboard exposing (Key(..))
import Note exposing(..)
import Update.Helper

type alias Model = FrontendModel

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
        Update.Helper.editNote model

    else
        model


makeNewNoteUsingKey : List Key -> Model -> Model
makeNewNoteUsingKey pressedKeys model =
    if List.member (Character "N") pressedKeys then
        Update.Helper.makeNewNote model

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
