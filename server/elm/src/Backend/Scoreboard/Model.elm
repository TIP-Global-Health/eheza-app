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
    , nutrition : NutritionCriterionsData
    , postpartumCheckups : Bool
    , ironDuringPregnancy : Bool
    }


type alias NutritionCriterionsData =
    { stunting : CriterionBySeverities
    , underweigt : CriterionBySeverities
    , wasting : CriterionBySeverities
    , muac : CriterionBySeverities
    }


type alias CriterionBySeverities =
    { severe : List NominalDate
    , moderate : List NominalDate
    , normal : List NominalDate
    }


type Msg
    = SetData Value
