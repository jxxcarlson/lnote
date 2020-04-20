module Config exposing(Config, config)


type alias Config =
    { debounceInterval : Float
    , timeoutInMs : Int
    , panelHeight : Float
    , panelWidth : Float
    }


config : Config
config =
    { debounceInterval = 455
    , timeoutInMs = 6 * 1000
    , panelHeight = 550
    , panelWidth = 450
    }
