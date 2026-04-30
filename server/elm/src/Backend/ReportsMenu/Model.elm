module Backend.ReportsMenu.Model exposing (MenuData, Msg(..))

import App.Types exposing (Site)
import Backend.Components.Model exposing (HealthCenterData, MenuScope)
import Json.Encode exposing (Value)


type alias MenuData =
    { site : Site
    , healthCenters : List HealthCenterData
    , scope : Maybe MenuScope
    }


type Msg
    = SetData Value
