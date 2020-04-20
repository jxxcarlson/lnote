module Evergreen.V24.Types exposing (..)

import Browser
import Debounce
import Evergreen.V24.FrequencyDict as FrequencyDict
import Http
import Keyboard
import Lamdera
import Evergreen.V24.Note as Note
import Random
import Set
import Time
import Url
import Evergreen.V24.User as User


type NotesMode
    = BrowsingNotes
    | CreatingNote
    | EditingNote


type ValidationState
    = SignInState
    | SignUpState
    | ChangePasswordState


type AppMode
    = UserNotes NotesMode
    | UserValidation ValidationState
    | Admin


type DeleteNoteSafety
    = DeleteNoteSafetyOn
    | DeleteNoteSafetyOff


type alias FrontendModel =
    { input : String
    , appMode : AppMode
    , pressedKeys : (List Keyboard.Key)
    , manualVisible : Bool
    , message : String
    , counter : Int
    , currentTime : Time.Posix
    , bodyDebouncer : (Debounce.Debounce String)
    , subjectDebouncer : (Debounce.Debounce String)
    , tagDebouncer : (Debounce.Debounce String)
    , currentUser : (Maybe User.User)
    , username : String
    , password : String
    , newPassword1 : String
    , newPassword2 : String
    , email : String
    , userList : (List User.User)
    , notes : (List Note.Note)
    , maybeCurrentNote : (Maybe Note.Note)
    , frequencyDict : FrequencyDict.FrequencyDict
    , newSubject : String
    , changedSubject : String
    , noteBody : String
    , tagString : String
    , noteFilterString : String
    , textFilterString : String
    , tagFilterString : String
    , selectedTag : String
    , noteCameBeforeString : String
    , noteCameAfterString : String
    , deleteNoteSafety : DeleteNoteSafety
    , uuid : (Maybe String)
    }


type alias BackendModel =
    { passwordDict : User.PasswordDict
    , userDict : (User.UserDict Note.Note)
    , clients : (Set.Set Lamdera.ClientId)
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : (Maybe Int)
    }


type FrontendMsg
    = FENoop
    | DebounceBody Debounce.Msg
    | DebounceSubject Debounce.Msg
    | DebounceTags Debounce.Msg
    | KeyboardMsg Keyboard.Msg
    | SendUsers
    | SetAppMode AppMode
    | TimeChange Time.Posix
    | SetManualVislble Bool
    | GotUserName String
    | GotPassword String
    | GotNewPassword1 String
    | GotNewPassword2 String
    | ChangePassword
    | GotEmail String
    | SignIn
    | SignUp
    | SignOut
    | ChangeUrl Url.Url
    | ClickLink Browser.UrlRequest
    | DownloadNotes
    | GotChangedSubject String
    | GotNoteBody String
    | GotNewNoteName String
    | GotTagString String
    | SetCurrentNote Note.Note
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


type ToBackend
    = NoOpToBackend
    | ClientJoin
    | RequestUsers
    | SendSignInInfo String String
    | SendSignUpInfo String String String
    | SendChangePasswordInfo User.Username String String
    | CreateNote (Maybe User.User) Note.Note
    | RequestNotes (Maybe User.User)
    | DeleteNote (Maybe User.User) Note.Note
    | UpdateNote (Maybe User.User) Note.Note
    | UpdateTags (Maybe User.User) Note.Note


type BackendMsg
    = NoOpBackendMsg
    | GotAtomsphericRandomNumber (Result Http.Error String)


type ToFrontend
    = NoOpToFrontend
    | SendMessage String
    | SendValidatedUser (Maybe User.User)
    | SendFrequencyDict FrequencyDict.FrequencyDict
    | SendUserList (List User.User)
    | SendNotesToFrontend (List Note.Note)
    | SendNoteToFrontend Note.Note