module Backend.ScoreboardMenu.Model exposing (..)

import App.Types exposing (Site)
import Json.Encode exposing (Value)


type alias MenuData =
    { site : Site
    }


type Msg
    = SetData Value
