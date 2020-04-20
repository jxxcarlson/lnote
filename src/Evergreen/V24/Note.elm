module Evergreen.V24.Note exposing (..)

import Time


type alias Note = 
    { id : String
    , subject : String
    , tags : (List String)
    , body : String
    , timeCreated : Time.Posix
    , timeModified : Time.Posix
    , selected : Bool
    }