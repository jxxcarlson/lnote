module Evergreen.V1.Types exposing
    ( AppMode(..)
    , BackendModel
    , BackendMsg(..)
    , DeleteNoteSafety(..)
    , FrontendModel
    , FrontendMsg(..)
    , NotesMode(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ValidationState(..)
    )

import Browser exposing (UrlRequest)
import Debounce exposing (Debounce)
import FrequencyDict exposing (FrequencyDict)
import Http
import Keyboard exposing (Key(..))
import Lamdera exposing (ClientId)
import Note exposing (Note)
import Random
import Set exposing (Set)
import Time exposing (Posix)
import TypedTime exposing (..)
import UUID exposing (UUID)
import Url exposing (Url)
import User exposing (PasswordDict, User, UserDict)
import UserData


type alias BackendModel =
    { passwordDict : PasswordDict
    , userDict : UserDict Note
    , clients : Set ClientId
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : Maybe Int
    }


type alias FrontendModel =
    { input : String
    , appMode : AppMode
    , pressedKeys : List Key
    , manualVisible : Bool
    , message : String
    , counter : Int
    , currentTime : Posix
    , bodyDebouncer : Debounce String
    , subjectDebouncer : Debounce String
    , tagDebouncer : Debounce String

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
    , frequencyDict : FrequencyDict
    , newSubject : String
    , changedSubject : String
    , noteBody : String
    , tagString : String
    , noteFilterString : String
    , textFilterString : String
    , tagFilterString : String
    , noteCameBeforeString : String
    , noteCameAfterString : String
    , deleteNoteSafety : DeleteNoteSafety
    , uuid : Maybe String
    }


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
    | GotAtomsphericRandomNumber (Result Http.Error String)



-- | XX SentToFrontendResult ClientId (Result WsError ())


type FrontendMsg
    = FENoop
    | DebounceBody Debounce.Msg
    | DebounceSubject Debounce.Msg
    | DebounceTags Debounce.Msg
    | KeyboardMsg Keyboard.Msg
      -- Admin
    | SendUsers
      -- App
    | SetAppMode AppMode
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
    | FEEditNote
    | DoUpdateNote
    | GotNoteDateBeforeFilter String
    | GotNoteDateAfterFilter String
    | GotNoteFilter String
    | GotTextFilter String
    | GotTagFilter String
    | SetTagForSearch String
    | ClearAllSearches
    | GetRandomNotes
    | SelectRandomNotes (List Int)
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
