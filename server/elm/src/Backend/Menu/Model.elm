module Backend.Menu.Model exposing (..)

import AssocList as Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type Site
    = SiteRwanda
    | SiteBurundi
    | SiteUnknown


type alias MenuData =
    { site : Site
    }


type Msg
    = SetData Value
