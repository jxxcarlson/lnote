module Evergreen.Migrate.V8 exposing (backendModel, backendMsg, frontendModel, frontendMsg, toBackend, toFrontend)

import Evergreen.Type.V7 as Old
import Evergreen.Type.V8 as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigration ModeUnchanged MsgUnchanged


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigration ModeUnchanged MsgUnchange


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsglMigration MsgUnchange MsgUnchange


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsglMigration MsgUnchange MsgUnchange


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsglMigration MsgUnchange MsgUnchange


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsglMigration MsgUnchange MsgUnchange
