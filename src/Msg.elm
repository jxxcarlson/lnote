module Msg exposing
    ( AppMode(..)
    , BackendMsg(..)
    , DeleteNoteSafety(..)
    , FrontendMsg(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ValidationState(..)
    )

import Browser exposing (UrlRequest(..))
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
    | SendNoteToBackend (Maybe User) Note


type ToFrontend
    = NoOpToFrontend
    | SendMessage String
    | SendValidatedUser (Maybe User)
    | SendUserList (List User)
    | SendNotesToFrontend (List Note)


type BackendMsg
    = NoOpBackendMsg
    | SentToFrontendResult ClientId (Result WsError ())


type FrontendMsg
    = NoOpFrontendMsg
      -- Admin
    | SendUsers
      -- App
    | SetAppMode AppMode
    | SentToBackendResult (Result WsError ())
    | TimeChange Posix
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
    | GotChangedNoteName String
    | GotNoteBody String
    | GotNewNoteName String
    | SetCurrentNote Note
    | DeleteCurrentNote
    | MakeNewNote
    | GotNoteDateBeforeFilter String
    | GotNoteDateAfterFilter String
    | GotNoteFilter String
    | SetDeleteNoteSafety DeleteNoteSafety


type AppMode
    = UserMode
    | UserValidation ValidationState
    | Admin


type DeleteNoteSafety
    = DeleteNoteSafetyOn
    | DeleteNoteSafetyOff


type ValidationState
    = SignInState
    | SignUpState
    | ChangePasswordState