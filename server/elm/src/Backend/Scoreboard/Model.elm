module Backend.Scoreboard.Model exposing (..)

import AssocList
import Dict exposing (Dict)
import Json.Encode exposing (Value)


type alias ScoreboardData =
    { entityName : String
    , entityType : SelectedEntity
    }


type SelectedEntity
    = EntityDistrict
    | EntitySector
    | EntityCell
    | EntityVillage


type Msg
    = SetData Value
