module Backend.ReportsMenu.Model exposing (..)

import App.Types exposing (Site)
import Backend.Components.Model exposing (HealthCenterData)
import Json.Encode exposing (Value)


type alias MenuData =
    { site : Site
    , healthCenters : List HealthCenterData
    }


type Msg
    = SetData Value
