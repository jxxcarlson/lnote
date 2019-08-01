module Msg exposing
    ( AppMode(..)
    , BackendMsg(..)
    , DeleteNoteSafety(..)
    , FrontendMsg(..)
    , NotesMode(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ValidationState(..)
    )

import Browser exposing (UrlRequest(..))
import Debounce
import FrequencyDict exposing (FrequencyDict)
import Lamdera.Types exposing (ClientId, WsError)
import Note exposing (Note)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (User)
import UserData


type ToBackend
    = NoOpToBackend
    | ClientJoin
    | RequestUsers
    | SendSignInInfo String String
    | SendSignUpInfo String String String
    | SendChangePasswordInfo User.Username String String
    | CreateNote (Maybe User) Note
    | RequestNotes (Maybe User)
    | DeleteNote (Maybe User) Note
    | UpdateNote (Maybe User) Note
    | UpdateTags (Maybe User) Note


type ToFrontend
    = NoOpToFrontend
    | SendMessage String
    | SendValidatedUser (Maybe User)
    | SendFrequencyDict FrequencyDict
    | SendUserList (List User)
    | SendNotesToFrontend (List Note)
    | SendNoteToFrontend Note


type BackendMsg
    = NoOpBackendMsg
    | SentToFrontendResult ClientId (Result WsError ())


type FrontendMsg
    = FENoop
    | DebounceBody Debounce.Msg
    | DebounceSubject Debounce.Msg
    | DebounceTags Debounce.Msg
      -- Admin
    | SendUsers
      -- App
    | SetAppMode AppMode
    | SentToBackendResult (Result WsError ())
    | TimeChange Posix
    | SetManualVislble Bool
      -- User
    | GotUserName String
    | GotPassword String
    | GotNewPassword1 String
    | GotNewPassword2 String
    | ChangePassword
    | GotEmail String
    | SignIn
    | SignUp
    | SignOut
      -- Url
    | ChangeUrl Url
    | ClickLink UrlRequest
      -- NOTES
    | DownloadNotes
    | GotChangedSubject String
    | GotNoteBody String
    | GotNewNoteName String
    | GotTagString String
    | SetCurrentNote Note
    | DeleteCurrentNote
    | MakeNewNote
    | FECreateNote
    | FEEditNote
    | DoUpdateNote
    | GotNoteDateBeforeFilter String
    | GotNoteDateAfterFilter String
    | GotNoteFilter String
    | GotTextFilter String
    | GotTagFilter String
    | SetTagForSearch String
    | ClearAllSearches
    | SetDeleteNoteSafety DeleteNoteSafety


type AppMode
    = UserNotes NotesMode
    | UserValidation ValidationState
    | Admin


type NotesMode
    = BrowsingNotes
    | CreatingNote
    | EditingNote


type DeleteNoteSafety
    = DeleteNoteSafetyOn
    | DeleteNoteSafetyOff


type ValidationState
    = SignInState
    | SignUpState
    | ChangePasswordState
