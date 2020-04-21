module User exposing
    ( Password
    , PasswordDict
    , User
    , UserDict
    , UserInfo
    , Username
    , add
    , deleteUser
    , encrypt
    , fromDict
    , getData
    , validateChangePassword
    , validateSignUpInfo
    , validateUser
    )

import Crypto.HMAC exposing (sha256, sha512)
import Dict exposing (Dict)
import FrequencyDict exposing (FrequencyDict)


type alias Username =
    String


type alias Password =
    String


type alias PasswordDict =
    Dict Username Password


type alias UserDict a =
    Dict Username (UserInfo a)


type alias UserInfo a =
    { email : String, admin : Bool, counter : Int, tagDict : FrequencyDict, data : List a }


type alias User =
    { username : Username, email : String, admin : Bool }


fromDict : UserDict a -> Username -> Maybe User
fromDict userDict username =
    case Dict.get username userDict of
        Nothing ->
            Nothing

        Just userInfo ->
            Just { username = username, email = userInfo.email, admin = userInfo.admin }


getData : Username -> UserDict a -> Maybe (List a)
getData username dict =
    Dict.get username dict
        |> Maybe.map .data


encrypt : String -> String
encrypt str =
    Crypto.HMAC.digest sha512 "YoKO-mukti-yada-BlK#10&%F.7.910-hoH0" str


validatePassword : String -> String -> Bool
validatePassword password encryptedPassword =
    encrypt password == encryptedPassword



validateUser : PasswordDict -> Username -> String -> Bool
validateUser passwordDict username passWord =
    case Dict.get username passwordDict of
        Nothing ->
            False

        Just encryptedPassword ->
            validatePassword passWord encryptedPassword


validateSignUpInfo : Username -> String -> String -> List String
validateSignUpInfo username password email =
    []
        |> userNameLongEnough username
        |> passwordLongEnough password
        |> emailValid email


validateChangePassword : String -> String -> List String
validateChangePassword password1 password2 =
    []
        |> passwordsMatch password1 password2
        |> passwordLongEnough password1


passwordsMatch : String -> String -> List String -> List String
passwordsMatch password1 password2 errorList =
    case password1 == password2 of
        True ->
            errorList

        False ->
            "Passwords don't match" :: errorList


userNameLongEnough username errorList =
    case String.length username < 6 of
        True ->
            "Username must have at least six characters" :: errorList

        False ->
            errorList


passwordLongEnough password errorList =
    case String.length password < 6 of
        True ->
            "Password must have at least six characters" :: errorList

        False ->
            errorList


emailValid email errorList =
    case String.contains "@" email && String.length email > 3 of
        False ->
            "Email is not valid" :: errorList

        True ->
            errorList


add : String -> String -> String -> ( PasswordDict, UserDict a ) -> Result String ( PasswordDict, UserDict a )
add username password email ( passwordDict, userDict ) =
    case ( Dict.member username userDict, passwordErrors password ) of
        ( False, [] ) ->
            let
                newPasswordDict =
                    Dict.insert username (encrypt password) passwordDict

                newUserInfo =
                  if username == "jxxcarlson!" then
                    { email = email, admin = True, counter = 0, data = [], tagDict = Dict.empty }
                  else
                    { email = email, admin = False, counter = 0, data = [], tagDict = Dict.empty }


                newUserDict =
                    Dict.insert username newUserInfo userDict
            in
            Ok ( newPasswordDict, newUserDict )

        _ ->
            Err "user already exists"


passwordErrors : String -> List String
passwordErrors str =
    []


deleteUser : Username -> ( PasswordDict, UserDict a ) -> ( PasswordDict, UserDict a )
deleteUser username ( passwordDict, userDict ) =
    ( Dict.remove username passwordDict, Dict.remove username userDict )
