module Backend.Completion.Model exposing (..)

import App.Types exposing (Site)
import AssocList as Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type alias CompletionData =
    { site : Site
    , entityName : String
    , entityType : SelectedEntity
    , nutritionIndividualData : List (EncounterData NutritionActivity)
    }


type SelectedEntity
    = EntityGlobal
    | EntityHealthCenter


type alias EncounterData activity =
    { startDate : NominalDate
    , expectedActivities : List activity
    , completedActivities : List activity
    , takenBy : Maybe TakenBy
    }


type NutritionActivity
    = NutritionHeight
    | NutritionNutrition
    | NutritionPhoto
    | NutritionWeight
    | NutritionMUAC
    | NutritionContributingFactors
    | NutritionFollowUp
    | NutritionHealthEducation
    | NutritionSendToHC
    | NutritionNCDA


type TakenBy
    = TakenByNurse
    | TakenByCHW
    | TakenByUnknown


type Msg
    = SetData Value
