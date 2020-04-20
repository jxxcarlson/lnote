module View.Admin exposing (view)

import Config exposing (config)
import DateTime
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FrequencyDict
import Html exposing (Html)
import Note exposing (Note)
import Style
import Text
import Time exposing (Posix)
import Types exposing (AppMode(..), DeleteNoteSafety(..), FrontendModel, FrontendMsg(..), NotesMode(..), ValidationState(..))
import User exposing (User)
import Utility
import View.Button
import View.Utility exposing (showIf)


type alias Model =
    FrontendModel


view : Model -> Element FrontendMsg
view model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    adminView_ model user


adminView_ : Model -> User -> Element FrontendMsg
adminView_ model user =
    column Style.mainColumnX
        [ el [ Font.size 14 ] (text <| "Admin: " ++ user.username)
        , indexedTable
            [ spacing 4, Font.size 12, paddingXY 0 12, height (px 300), scrollbarY ]
            { data = model.userList
            , columns =
                [ { header = el [ Font.bold ] (text "k")
                  , width = px 40
                  , view = \k usr -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                  }
                , { header = el [ Font.bold ] (text "Username")
                  , width = px 130
                  , view = \k usr -> el [ Font.size 12 ] (text usr.username)
                  }
                , { header = el [ Font.bold ] (text "Email")
                  , width = px 200
                  , view = \k usr -> el [ Font.size 12 ] (text usr.email)
                  }
                ]
            }
        ]
