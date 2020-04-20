
module Update.Helper exposing(createNote, makeNewNote, editNote, newNote, selectNotes)


import Types exposing(FrontendModel,FrontendMsg, ToBackend(..), NotesMode(..), AppMode(..))
import Lamdera exposing (sendToBackend)
import Note exposing(Note)


type alias Model = FrontendModel

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
