module Evergreen.V24.User exposing (..)

import Dict
import Evergreen.V24.FrequencyDict as FrequencyDict


type alias Username = String


type alias User = 
    { username : Username
    , email : String
    , admin : Bool
    }


type alias Password = String


type alias PasswordDict = (Dict.Dict Username Password)


type alias UserInfo a = 
    { email : String
    , admin : Bool
    , counter : Int
    , tagDict : FrequencyDict.FrequencyDict
    , data : (List a)
    }


type alias UserDict a = (Dict.Dict Username (UserInfo a))