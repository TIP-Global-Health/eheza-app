module Backend.Scoreboard.Model exposing (..)

import AssocList
import Dict exposing (Dict)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type alias ScoreboardData =
    { entityName : String
    , entityType : SelectedEntity
    , records : List PatientData
    }


type SelectedEntity
    = EntityDistrict
    | EntitySector
    | EntityCell
    | EntityVillage


type alias PatientData =
    { birthDate : NominalDate }


type Msg
    = SetData Value
