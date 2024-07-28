module Backend.ReportsMenu.Model exposing (..)

import App.Types exposing (Site)
import Json.Encode exposing (Value)


type alias MenuData =
    { site : Site
    , healthCenters : List HealthCenterData
    }


type Msg
    = SetData Value


type alias HealthCenterData =
    { id : HealthCenterId
    , name : String
    }


type alias HealthCenterId =
    Int
