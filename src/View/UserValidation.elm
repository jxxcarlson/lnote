module View.UserValidation exposing (view)

import Config exposing (config)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Style
import Types exposing (AppMode(..), FrontendModel, FrontendMsg(..), ValidationState(..))
import User exposing (User)
import View.Utility exposing (showIf, hideIf)
import Widget.Button as Button exposing(Size(..))
import Widget.TextField as TextField


type alias Model =
    FrontendModel


view : Model -> Element FrontendMsg
view model =
    case model.currentUser of
        Nothing ->
            noUserView model

        Just user ->
            signedInUserView model user


noUserView : Model -> Element FrontendMsg
noUserView model =
    row [ spacing 12 ]
        [ noUserLHS model
        , description model
        , screenShot model
        ]


noUserLHS model =
  column [ padding 40, width (px 330), spacing 10, alignTop]
    -- column (Style.mainColumn (px 600)fill)
        [ el [ Font.size 18, Font.bold, paddingEach {nullPadding | bottom = 12}] (text "Welcome to Lamdera Notes")
        , inputUserName model
        , inputPassword model
        , showIf (model.appMode == UserValidation SignUpState) (inputEmail model)
        , showIf (model.appMode == UserValidation SignUpState) (el [ Font.size 12 ] (text "Email address optional"))
        , row [ spacing 12, paddingXY 0 12 ]
            [ showIf (model.appMode == UserValidation SignInState) (signInButton model)
            , row [ spacing 12 ]
                [ signUpButton model
                , showIf (model.appMode == UserValidation SignUpState) (cancelSignUpButton model)
                ]
            ]
        , el [ Font.size 16, Font.color Style.darkRed ] (text model.message)
        ]


nullPadding = { left = 0, right = 0, top = 0, bottom = 0}

screenShot model =
  column [ spacing 18, Font.size 16, paddingXY 0 40, width (pxFloat config.panelWidth), height (px 550), scrollbarY ]
      [  image
          [ width (pxFloat config.panelWidth) ]
          { src = "http://noteimages.s3.amazonaws.com/jim_images/notes-screen.png"
          , description = "screenshot of app"
          }

      ]

description model =
    column [ padding 40, spacing 18, Font.size 14, alignTop, width (pxFloat (0.8*config.panelWidth)), height (px 550), scrollbarY ]
        [  Element.paragraph []
            [ el [ Font.bold ] (text "Features. ")
            , text "Searchable note repository. Supports Markdown. Filter notes by title, tags, full text. "
            , text "Active links to the most-used tags. Notes are automatically saved every 0.5 second. "
            , text "Export to JSON to keep personal copy of all notes."
            ]
        , Element.paragraph [] [el [Font.bold] (text "Example. " )
        , text "Full text search on 'sci' returns articles with text containing 'science', search on 'sci po' returns articles containing 'science' and 'political', etc."
                       ]
        , Element.paragraph []
            [ el [ Font.bold ] (text "Coming soon. ")
            , text "Filter by date, options to sort note list."
            ]
        ]

adminStatus : Model -> Element FrontendMsg
adminStatus model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    el [ Font.size 12 ] (text "Admin")


pxFloat =
    round >> px




signedInUserView : Model -> User -> Element FrontendMsg
signedInUserView model user =
    column Style.mainColumnX
        [ el [] (text <| "Signed in as " ++ user.username)
        , hideIf (model.appMode == UserValidation ChangePasswordState) (signOutButton model)
        , showIf (model.appMode == UserValidation ChangePasswordState) (passwordPanel model)
        , row [ spacing 12 ]
            [ changePasswordButton model
            , showIf (model.appMode == UserValidation ChangePasswordState) (cancelChangePasswordButton model)
            ]
        , adminStatus model
        ]


passwordPanel model =
    column [ spacing 12, paddingXY 0 18 ]
        [ inputCurrentPassword model
        , inputNewPassword1 model
        , inputNewPassword2 model
        , el [ Font.size 16, Font.color Style.darkRed ] (text model.message)
        ]


inputCurrentPassword model =
    TextField.make GotPassword model.password "Old password"
        |> TextField.withHeight 30
        |> TextField.withWidth 200
        |> TextField.withLabelWidth 110
        |> TextField.toElement




inputNewPassword1 model =
      TextField.make GotNewPassword1 model.newPassword1 "New password"
          |> TextField.withHeight 30
          |> TextField.withWidth 200
          |> TextField.withLabelWidth 110
          |> TextField.toElement

inputNewPassword2 model =
        TextField.make GotNewPassword2 model.newPassword2 "Password again"
            |> TextField.withHeight 30
            |> TextField.withWidth 200
            |> TextField.withLabelWidth 110
            |> TextField.toElement





inputUserName model =
      TextField.make GotUserName model.username "Username"
          |> TextField.withHeight 35
          |> TextField.withWidth 200
          |> TextField.withLabelWidth 100
          |> TextField.toElement



inputEmail model =
        TextField.make GotEmail model.email "Email"
            |> TextField.withHeight 35
            |> TextField.withWidth 200
            |> TextField.withLabelWidth 100
            |> TextField.toElement



inputPassword model =
          TextField.make GotPassword model.password "Password"
              |> TextField.withHeight 35
              |> TextField.withWidth 200
              |> TextField.withLabelWidth 100
              |> TextField.toElement




signInButton : Model -> Element FrontendMsg
signInButton model =
  Button.make  SignIn "Sign in"
          |> Button.withWidth (Bounded 100)
          |> Button.toElement


signOutButton : Model -> Element FrontendMsg
signOutButton model =
  Button.make  SignOut "Sign out"
          |> Button.withWidth (Bounded 150)
          |> Button.toElement




signUpButton : Model -> Element FrontendMsg
signUpButton model =
  let
    msg =
      case model.appMode of
        UserValidation SignUpState -> SignUp
        _ -> SetAppMode (UserValidation SignUpState)
  in
  Button.make  msg "Sign up"
     |> Button.withWidth (Bounded 100)
     |> Button.toElement




cancelSignUpButton : Model -> Element FrontendMsg
cancelSignUpButton model =
  let
    msg = case model.appMode of
      UserValidation SignUpState ->
           (SetAppMode (UserValidation SignInState))

      _ ->
           FENoop
  in
  Button.make  msg "Cancel"
     |> Button.withWidth (Bounded 100)
     |> Button.toElement




changePasswordButton : Model -> Element FrontendMsg
changePasswordButton model =
  let
    msg =
      case model.appMode of
         UserValidation ChangePasswordState ->
              ChangePassword

         _ ->
              SetAppMode (UserValidation ChangePasswordState)
  in
  Button.make  msg "Change password"
     |> Button.withWidth (Bounded 150)
     |> Button.toElement


cancelChangePasswordButton : Model -> Element FrontendMsg
cancelChangePasswordButton model =
   let
     msg = case model.appMode of
       UserValidation ChangePasswordState ->
           SetAppMode (UserValidation SignInState)

       _ ->
           FENoop
   in
   Button.make  msg "Cancel"
      |> Button.withWidth (Bounded 100)
      |> Button.toElement




-- Utility
