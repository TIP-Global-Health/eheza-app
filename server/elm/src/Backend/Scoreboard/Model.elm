module Backend.Scoreboard.Model exposing (..)

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
    { birthDate : NominalDate
    , lowBirthWeight : Maybe Bool
    , stuntingSevere : List NominalDate
    , stuntingModerate : List NominalDate
    , stuntingNormal : List NominalDate
    , postpartumCheckups : Bool
    , ironDuringPregnancy : Bool
    }


type Msg
    = SetData Value
