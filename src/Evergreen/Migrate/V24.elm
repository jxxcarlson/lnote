module Evergreen.Migrate.V24 exposing (..)

import Evergreen.V1.Types as Old exposing(DeleteNoteSafety(..), AppMode(..))
import Evergreen.V24.Types as New
import Lamdera.Migrations exposing (..)
import Debounce
import Dict
import Time


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
     ModelMigrated
         ( { input = "App started"
        , message = "Please sign in"
        , pressedKeys = []
        , appMode = UserValidation SignInState
        , manualVisible = False
        , currentTime = Time.millisToPosix 0
        , counter = 0
        , bodyDebouncer = Debounce.init
        , subjectDebouncer = Debounce.init
        , tagDebouncer = Debounce.init

        -- ADMIN
        , -- USER
          currentUser = Nothing
        , username = ""
        , password = ""
        , newPassword1 = ""
        , newPassword2 = ""
        , email = ""
        , userList = []

        -- NOTES
        , notes = []
        , maybeCurrentNote = Nothing
        , frequencyDict = Dict.empty
        , noteBody = ""
        , tagString = ""
        , noteFilterString = ""
        , newSubject = ""
        , changedSubject = ""
        , textFilterString = ""
        , tagFilterString = ""
        , selectedTag = ""
        , deleteNoteSafety = DeleteNoteSafetyOn
        , noteCameBeforeString = ""
        , noteCameAfterString = ""
        , uuid = Nothing
      }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
